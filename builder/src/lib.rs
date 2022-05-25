use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned, ToTokens};
use std::collections::{HashMap, HashSet};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::PathArguments::AngleBracketed;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DeriveInput, Field, Fields,
    GenericArgument, Ident, Lit, Token, Type, TypePath,
};

struct Builder {
    struct_name: Ident,
    fields: Vec<Field>,
    option_fields: HashSet<Field>,
    each_fields: HashMap<Field, Ident>,
}

struct BuilderFieldMeta {
    each_func_name: Ident,
}

impl Parse for BuilderFieldMeta {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let span = input.span();
        let ident = input.parse::<Ident>()?;
        if ident != "each" {
            return Err(syn::Error::new(span, r#"expected `builder(each = "...")`"#));
        }
        let _ = input.parse::<Token!(=)>()?;
        let each_func_name = match input.parse::<Lit>()? {
            Lit::Str(s) => s.value(),
            _ => panic!("must use string literal"),
        };
        Ok(BuilderFieldMeta {
            each_func_name: format_ident!("{}", each_func_name),
        })
    }
}

impl Builder {
    fn new(input: DeriveInput) -> syn::Result<Self> {
        let struct_name = input.ident;
        let mut option_fields = HashSet::new();
        let mut each_fields = HashMap::new();
        let fields = match input.data {
            Data::Struct(data) => match data.fields {
                Fields::Named(fields) => {
                    for f in &fields.named {
                        let field_meta = f
                            .attrs
                            .first()
                            .map(|attr| {
                                let mut ts = proc_macro2::TokenStream::new();
                                attr.path.to_tokens(&mut ts);
                                attr.tokens.to_tokens(&mut ts);
                                attr.parse_args::<BuilderFieldMeta>()
                                    .map_err(|err| syn::Error::new_spanned(ts, err.to_string()))
                            })
                            .transpose()?;

                        if let Type::Path(ref p) = f.ty {
                            let TypePath { path, .. } = p;
                            if let Some(ps) = path.segments.first() {
                                if let Some(field_meta) = field_meta {
                                    assert_eq!(
                                        ps.ident, "Vec",
                                        "`each` meta only works on Vec type"
                                    );
                                    each_fields.insert(f.clone(), field_meta.each_func_name);
                                } else if ps.ident == "Option" {
                                    option_fields.insert(f.clone());
                                }
                            }
                        }
                    }
                    fields.named.iter().cloned().collect()
                }
                Fields::Unnamed(_) => unimplemented!(),
                Fields::Unit => unimplemented!(),
            },
            Data::Enum(_) => unimplemented!(),
            Data::Union(_) => unimplemented!(),
        };

        Ok(Self {
            struct_name,
            fields,
            option_fields,
            each_fields,
        })
    }

    fn get_builder_tt(&self) -> TokenStream {
        let builder_name = format_ident!("{}Builder", self.struct_name.to_string());

        let struct_name = self.struct_name.clone();

        let fields = self.fields.iter().map(|f| {
            let name = &f.ident;
            let name = name.as_ref().map(|n| format_ident!("_{}", n.to_string()));
            let typ = &f.ty;
            if self.option_fields.contains(f) {
                quote_spanned! {f.span()=>
                    #name: #typ,
                }
            } else {
                quote_spanned! {f.span()=>
                    #name: std::option::Option<#typ>,
                }
            }
        });

        let funcs = self.fields.iter().map(|f| {
            let struct_field = &f.ident;
            let builder_field = struct_field
                .as_ref()
                .map(|n| format_ident!("_{}", n.to_string()));
            let typ = &f.ty;
            if let Some(func_name) = self.each_fields.get(f) {
                let mut inner_typ = None;
                if let Type::Path(ref p) = typ {
                    let TypePath { path, .. } = p;
                    if let Some(ps) = path.segments.first() {
                        if let AngleBracketed(ref a) = ps.arguments {
                            let AngleBracketedGenericArguments { args, .. } = a;
                            if let Some(GenericArgument::Type(ty)) = args.first() {
                                inner_typ = Some(ty);
                            }
                        }
                    }
                }
                assert!(inner_typ.is_some(), "Wrong Vec type");
                let inner_typ = inner_typ.unwrap();

                quote_spanned! {f.span()=>
                    fn #func_name(&mut self, val: #inner_typ) -> &mut Self {
                        if let Some(arr) = self.#builder_field.as_mut() {
                            arr.push(val);
                        } else {
                            self.#builder_field = Some(vec![val]);
                        }
                        self
                    }
                }
            } else if self.option_fields.contains(f) {
                let mut inner_typ = None;
                if let Type::Path(ref p) = typ {
                    let TypePath { path, .. } = p;
                    if let Some(ps) = path.segments.first() {
                        if let AngleBracketed(ref a) = ps.arguments {
                            let AngleBracketedGenericArguments { args, .. } = a;
                            if let Some(GenericArgument::Type(ty)) = args.first() {
                                inner_typ = Some(ty);
                            }
                        }
                    }
                }
                assert!(inner_typ.is_some(), "Wrong option type");
                let inner_typ = inner_typ.unwrap();

                quote_spanned! {f.span()=>
                    fn #struct_field(&mut self, val: #inner_typ) -> &mut Self {
                        self.#builder_field = std::option::Option::Some(val);
                        self
                    }
                }
            } else {
                quote_spanned! {f.span()=>
                    fn #struct_field(&mut self, val: #typ) -> &mut Self {
                        self.#builder_field = std::option::Option::Some(val);
                        self
                    }
                }
            }
        });

        let assigns = self.fields.iter().map(|f| {
            let struct_field = &f.ident;
            let builder_field = struct_field
                .as_ref()
                .map(|n| format_ident!("_{}", n.to_string()));

            let s = struct_field.as_ref().unwrap().to_string();
            if self.each_fields.contains_key(f) {
                quote_spanned! {f.span()=>
                    #struct_field: self.#builder_field.clone().unwrap_or_else(|| vec![]),
                }
            } else if self.option_fields.contains(f) {
                quote_spanned! {f.span()=>
                    #struct_field: self.#builder_field.clone(),
                }
            } else {
                quote_spanned! {f.span()=>
                    #struct_field: self.#builder_field.clone().ok_or_else(|| format!("{} is not inited", #s))?,
                }

            }
        });

        let expanded = quote! {
            impl #struct_name {
                fn builder() -> #builder_name {
                    #builder_name::default()
                }
            }

            #[derive(Default)]
            struct #builder_name {
                #(#fields)*
            }

            impl #builder_name {
                #(#funcs)*

                fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                    let s = #struct_name {
                        #(#assigns)*
                    };
                    Ok(s)
                }
            }
        };
        TokenStream::from(expanded)
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);
    match Builder::new(derive_input) {
        Ok(builder) => builder.get_builder_tt(),
        Err(err) => TokenStream::from(err.to_compile_error()),
    }
}
