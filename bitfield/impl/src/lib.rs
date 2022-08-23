use proc_macro::TokenStream;
use proc_macro2::{Span, TokenTree};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse::Parser, parse_macro_input, spanned::Spanned, DeriveInput, Fields, Ident, ItemStruct,
    Type,
};

struct BitField {
    gen_result: TokenStream,
}

impl BitField {
    fn new(mut item_struct: ItemStruct) -> syn::Result<Self> {
        if let Fields::Named(fields_named) = &item_struct.fields {
            let struct_name = item_struct.ident.clone();
            let mut filed_names = vec![];
            let mut bits = vec![];
            let mut types = vec![];
            let mut check_eq_bits = vec![];
            for field in &fields_named.named {
                let mut user_bit = None;
                for attr in &field.attrs {
                    if "bits" == &attr.path.segments[0].ident.to_string() {
                        let tokens: Vec<_> = attr.tokens.clone().into_iter().collect();
                        match (tokens.get(0), tokens.get(1)) {
                            (Some(TokenTree::Punct(p)), Some(TokenTree::Literal(lit)))
                                if p.as_char() == '=' =>
                            {
                                user_bit = Some(lit.clone());
                                break;
                            }
                            _ => {
                                return Err(syn::Error::new(
                                    attr.tokens.span(),
                                    "unexpected tokens",
                                ));
                            }
                        }
                    }
                }

                filed_names.push(field.ident.clone().unwrap());
                if let Type::Path(type_path) = &field.ty {
                    let segs = type_path.path.segments.clone();
                    bits.push(quote! {
                        <#segs as Specifier>::BITS
                    });
                    types.push(quote! {
                        <#segs as Specifier>::TheType
                    });
                    if let Some(user_bit) = user_bit {
                        let type_bit = bits.last().cloned().unwrap();
                        let ident = Ident::new(
                            &format!("_Check{}UserBitEqTypeBit", &field.ident.clone().unwrap()),
                            field.ident.span(),
                        );
                        let func_ident = Ident::new(
                            &format!(
                                "_Check{}UserBitEqTypeBitFunc",
                                &field.ident.clone().unwrap()
                            ),
                            field.ident.span(),
                        );

                        check_eq_bits.push(quote_spanned! {user_bit.span()=>
                            #[allow(non_upper_case_globals, non_snake_case)]
                            const fn #func_ident(_v: [(); #user_bit]) {}

                            #[allow(non_upper_case_globals)]
                            const #ident: () = #func_ident([(); #type_bit]);

                            // const #ident: CheckUserBitEqTypeBit::<BoolType::<{#s}>> = CheckUserBitEqTypeBit {_marker: std::marker::PhantomData};
                        });
                    };
                } else {
                    return Err(syn::Error::new(Span::call_site(), "incorrect bit type"));
                }
            }
            let bit_cnt = quote! {
               #(#bits)+*
            };
            let byte_cnt = quote!(
                (#(#bits)+*)  / 8
            );
            let s = quote! {
                {
                    data: [u8; #byte_cnt]
                }
            }
            .into();
            item_struct.fields = Fields::Named(syn::parse(s).unwrap());
            let s = quote! {
                #[repr(C)]
            }
            .into();
            item_struct.attrs = syn::Attribute::parse_outer.parse(s).unwrap();

            let mut set_get = vec![];

            for (i, ((name, _bit), the_type)) in filed_names
                .iter()
                .zip(bits.iter())
                .zip(types.iter())
                .enumerate()
            {
                let get_name =
                    syn::Ident::new(&format!("get_{}", name.to_string()), Span::call_site());
                let set_name =
                    syn::Ident::new(&format!("set_{}", name.to_string()), Span::call_site());
                let start_idx = if i == 0 {
                    quote! {0}
                } else {
                    let slice = &bits[..i];
                    quote! {
                        #(#slice)+*
                    }
                };
                let end_idx = {
                    let slice = &bits[..=i];
                    quote! {
                        #(#slice)+*
                    }
                };

                let q = quote! {
                    fn #get_name(&self) -> #the_type {
                        let start_idx = #start_idx;
                        let end_idx = #end_idx;
                        let mut res = 0;
                        for i in (start_idx..end_idx).rev() {
                            let b = (self.data[i / 8] >> (i % 8)) & 0b1;
                            res = (res << 1) | (b as u64);
                        }
                        return unsafe { std::mem::transmute_copy(&res) };
                    }

                    fn #set_name(&mut self, val: #the_type) {
                        let mut val: u64 = unsafe { std::mem::transmute_copy(&val) };
                        let start_idx = #start_idx;
                        let end_idx = #end_idx;
                        for i in start_idx..end_idx {
                            let b = (val & 0b1) as u8;
                            val >>= 1;
                            self.data[i / 8] &= !(1 << i % 8);
                            self.data[i / 8] |= b << i % 8;
                        }
                    }
                };
                set_get.push(q);
            }

            let item_struct = item_struct.to_token_stream();

            let gen_result = quote! {
                const _CheckTotalSize: _CheckTotalSize<_TotalSize<{ #bit_cnt }>> = _CheckTotalSize {_marker: std::marker::PhantomData};

                #item_struct

                #(#check_eq_bits)*

                impl #struct_name {
                    fn new() -> Self {
                        assert_eq!((#bit_cnt) % 8, 0);
                        Self {
                            data: [0; #byte_cnt]
                        }
                    }

                    #(#set_get)*
                }
            }
            .into();

            Ok(Self { gen_result })
        } else {
            Err(syn::Error::new(
                Span::call_site(),
                "only support named struct",
            ))
        }
    }
}

#[proc_macro_attribute]
pub fn bitfield(_args: TokenStream, input: TokenStream) -> TokenStream {
    let item_struct = parse_macro_input!(input as ItemStruct);
    let bf = BitField::new(item_struct).unwrap();
    bf.gen_result
}

#[proc_macro]
pub fn gen_bit_enums(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let x = (1..=64).map(|i| {
        let num = proc_macro2::Literal::u16_unsuffixed(i);
        let ident = syn::Ident::new(&format!("B{}", i), Span::call_site());
        let &t = [8, 16, 32, 64]
            .iter()
            .find(|&&v| v >= ((i - 1) / 8 + 1) * 8)
            .unwrap_or_else(|| &64);

        let t = format!("u{}", t);
        let s: syn::Type = syn::parse_str(&t).unwrap();
        quote! {
            pub enum #ident {}

            impl Specifier for #ident {
                const BITS: usize = #num;
                type TheType = #s;
            }
        }
    });
    proc_macro2::TokenStream::from_iter(x).into()
}

#[proc_macro]
pub fn gen_mod_8_trait(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut s = vec![quote! {
        pub struct _CheckTotalSize<T: TotalSizeIsMultipleOfEightBits>
        {
            pub _marker: std::marker::PhantomData<T>
        }

        pub struct _TotalSize<const N: usize> {}
    }];
    let x = (1..=64).filter(|i| i % 8 == 0).map(|i| {
        let num = proc_macro2::Literal::usize_unsuffixed(i);
        quote! {
            impl TotalSizeIsMultipleOfEightBits for _TotalSize<#num> {}
        }
    });
    s.extend(x);
    proc_macro2::TokenStream::from_iter(s).into()
}

struct BitfieldSpecifier {}

impl BitfieldSpecifier {
    fn new(derive_input: DeriveInput) -> syn::Result<TokenStream> {
        let enum_name = derive_input.ident;
        match derive_input.data {
            syn::Data::Enum(data_enum) => {
                let l = data_enum.variants.len();
                if l == 0 || ((l - 1) & l != 0) {
                    return Err(syn::Error::new(
                        Span::call_site(),
                        "BitfieldSpecifier expected a number of variants which is a power of 2",
                    ));
                }

                let maxi = proc_macro2::Literal::usize_unsuffixed(l);
                let mut cur_val = vec![];
                let mut result = vec![];

                for (i, variant) in data_enum.variants.iter().enumerate() {
                    if let Some((_, expr)) = &variant.discriminant {
                        cur_val = vec![quote! { #expr }];
                    } else {
                        if i == 0 {
                            cur_val.push(quote! { 0 });
                        } else {
                            cur_val.push(quote! { 1 });
                        }
                    }
                    let s = quote! { (0 <= #(#cur_val)+*) && (((#(#cur_val)+*) as usize) < #maxi) };
                    let ident = Ident::new(
                        &format!("_Check{}IsDiscriminantInRange", &variant.ident),
                        variant.ident.span(),
                    );
                    result.push(quote_spanned! {variant.ident.span()=>
                        #[allow(non_upper_case_globals)]
                        const #ident: CheckIsDiscriminantInRange::<BoolType::<{#s}>> = CheckIsDiscriminantInRange {_marker: std::marker::PhantomData};
                    });
                }

                let bits = (l as f64).log2() as usize;
                let bits = proc_macro2::Literal::usize_unsuffixed(bits);
                result.push(quote! {
                    impl Specifier for #enum_name {
                        const BITS: usize = #bits;
                        type TheType = #enum_name;
                    }
                });

                Ok(proc_macro2::TokenStream::from_iter(result).into())
            }
            _ => todo!(),
        }
    }
}

#[proc_macro_derive(BitfieldSpecifier)]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);
    match BitfieldSpecifier::new(derive_input) {
        Ok(s) => s,
        Err(err) => TokenStream::from(err.to_compile_error()),
    }
}
