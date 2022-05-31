use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::{quote, quote_spanned};
use std::collections::HashMap;
use std::str::FromStr;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    spanned::Spanned,
    Data, DeriveInput, Field, Fields, GenericArgument, GenericParam, Generics, Ident, Lit, Meta,
    PathArguments, Token, Type, TypePath,
};

struct CustomDebug {
    struct_name: Ident,
    fields: Vec<Field>,
    format_fields: HashMap<Field, String>,
    generics: Generics,
    bound_generics: Vec<GenericParam>,
    raw_generics: Generics,
    associated_types: Vec<TypePath>,
    attr_bound_lits: Vec<proc_macro2::TokenStream>,
}

struct Attr {
    bound_lit: proc_macro2::TokenStream,
}

impl Parse for Attr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let _ = input.parse::<Ident>()?;
        let _ = input.parse::<Token!(=)>()?;
        let bound_lit = match input.parse::<Lit>()? {
            Lit::Str(s) => {
                let t = proc_macro2::TokenStream::from_str(&s.value())?;
                t
            }
            _ => panic!("must use string literal"),
        };
        Ok(Self { bound_lit })
    }
}

impl CustomDebug {
    fn new(derive_input: DeriveInput) -> syn::Result<Self> {
        let mut attr_bound_lits = vec![];
        for attr in derive_input.attrs {
            if let Some(ident) = attr.path.get_ident() {
                if ident == "debug" {
                    let attr = attr.parse_args::<Attr>()?;
                    attr_bound_lits.push(attr.bound_lit);
                }
            }
        }
        let struct_name = derive_input.ident;
        let mut format_fields = HashMap::new();
        let generics = derive_input.generics.clone();
        let mut bound_generics = generics
            .params
            .iter()
            .filter(|g| {
                if let GenericParam::Type(tp) = g {
                    tp.bounds.is_empty()
                } else {
                    true
                }
            })
            .cloned()
            .collect::<Vec<_>>();

        let mut raw_generics = generics.clone();
        for tp in raw_generics.type_params_mut() {
            tp.bounds.clear();
        }
        let mut associated_types = Vec::new();

        let fields = match derive_input.data {
            Data::Struct(data) => match data.fields {
                Fields::Named(fields) => {
                    for field in &fields.named {
                        if let Type::Path(tp) = &field.ty {
                            for path_seg in &tp.path.segments {
                                if let PathArguments::AngleBracketed(args) = &path_seg.arguments {
                                    if path_seg.ident == "PhantomData" {
                                        for ty in &args.args {
                                            if let GenericArgument::Type(Type::Path(tp)) = ty {
                                                if let Some(ident) = tp.path.get_ident() {
                                                    let mut remove_idx = vec![];
                                                    for i in 0..bound_generics.len() {
                                                        if let GenericParam::Type(tp) =
                                                            &bound_generics[i]
                                                        {
                                                            if &tp.ident == ident {
                                                                remove_idx.push(i)
                                                            }
                                                        }
                                                    }
                                                    for i in remove_idx.into_iter().rev() {
                                                        bound_generics.swap_remove(i);
                                                    }
                                                }
                                            }
                                        }
                                    } else {
                                        for ty in &args.args {
                                            if let GenericArgument::Type(Type::Path(tp)) = ty {
                                                if tp.path.segments.len() > 1 {
                                                    let ident = &tp.path.segments[0].ident;
                                                    let mut remove_idx = vec![];
                                                    for i in 0..bound_generics.len() {
                                                        if let GenericParam::Type(tp) =
                                                            &bound_generics[i]
                                                        {
                                                            if &tp.ident == ident {
                                                                remove_idx.push(i)
                                                            }
                                                        }
                                                    }
                                                    for i in remove_idx.into_iter().rev() {
                                                        bound_generics.swap_remove(i);
                                                    }
                                                    associated_types.push(tp.clone());
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        field.attrs.first().map(|attr| {
                            attr.parse_meta().map(|val| match val {
                                Meta::NameValue(val) => match val.lit {
                                    Lit::Str(f) => {
                                        format_fields.insert(field.clone(), f.value());
                                    }
                                    _ => unimplemented!(),
                                },
                                Meta::Path(_) => unimplemented!(),
                                Meta::List(_) => unimplemented!(),
                            })
                        });
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
            format_fields,
            generics,
            bound_generics,
            raw_generics,
            associated_types,
            attr_bound_lits,
        })
    }

    fn gen_impl_debug(&self) -> TokenStream {
        let struct_name = &self.struct_name;
        let struct_name_lit = Literal::string(&*struct_name.to_string());

        let format_quotes = self.fields.iter().enumerate().map(|(i, f)| {
            let ident = f.ident.as_ref().unwrap();
            let format_form = if let Some(l) = self.format_fields.get(f) {
                if i + 1 == self.fields.len() {
                    Literal::string(&format!("{}: {}", ident, l))
                } else {
                    Literal::string(&format!("{}: {}, ", ident, l))
                }
            } else if i + 1 == self.fields.len() {
                Literal::string(&format!("{}: {{:#?}}", ident))
            } else {
                Literal::string(&format!("{}: {{:#?}}, ", ident))
            };
            quote_spanned! {f.span()=>
                write!(f, #format_form, self.#ident)?;
            }
        });

        let generics = &self.generics;
        let raw_generics = &self.raw_generics;
        let attr_bound_lits = &self.attr_bound_lits;

        let where_clauses = if generics.params.is_empty() {
            if attr_bound_lits.is_empty() {
                quote! {}
            } else {
                quote! {
                    where: #(#attr_bound_lits)*
                }
            }
        } else {
            let trait_bounds = generics
                .params
                .iter()
                .map(|gp| {
                    let gp = gp.clone();
                    let mut gp_ident = String::new();
                    if let GenericParam::Type(ref tp) = gp {
                        gp_ident = tp.ident.to_string();
                    }

                    if self.bound_generics.iter().any(|tp| {
                        if let GenericParam::Type(tp) = &tp {
                            tp.ident == gp_ident
                        } else {
                            false
                        }
                    }) {
                        if let GenericParam::Type(tp) = &gp {
                            quote! {
                                #tp: std::fmt::Debug,
                            }
                        } else {
                            quote! {}
                        }
                    } else {
                        quote! {}
                    }
                })
                .collect::<Vec<_>>();
            let at_bounds = self
                .associated_types
                .iter()
                .map(|at| {
                    quote! {
                        #at: std::fmt::Debug,
                    }
                })
                .collect::<Vec<_>>();

            if trait_bounds.is_empty() && at_bounds.is_empty() && attr_bound_lits.is_empty() {
                quote! {}
            } else {
                let attr = attr_bound_lits;
                quote! {
                    where #(#trait_bounds)* #(#at_bounds)* #(#attr)*
                }
            }
        };

        let q = quote! {
            impl #generics std::fmt::Debug for #struct_name #raw_generics
            #where_clauses
            {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{} {{ ", #struct_name_lit)?;
                    #(#format_quotes)*
                    write!(f, " }}")
                    // f.debug_struct(#struct_name_lit)
                    //     #(.#fields)*
                    //     .finish()
                }
            }
        };
        TokenStream::from(q)
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match CustomDebug::new(input) {
        Ok(custom_debug) => custom_debug.gen_impl_debug(),
        Err(err) => TokenStream::from(err.to_compile_error()),
    }
}
