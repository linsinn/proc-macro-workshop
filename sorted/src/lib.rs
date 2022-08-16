use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::spanned::Spanned;
use syn::visit_mut::{self, VisitMut};
use syn::{parse_macro_input, Item, ItemFn};

struct NameSpan {
    name: String,
    span: Span,
}

impl NameSpan {
    fn new(name: String, span: Span) -> Self {
        Self { name, span }
    }

    fn from<T>(t: T) -> Self
    where
        T: std::fmt::Display,
        T: Spanned,
    {
        Self {
            name: t.to_string(),
            span: t.span(),
        }
    }
}

impl std::fmt::Display for NameSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

struct Sorted {
    names: Vec<NameSpan>,
    visit_err: Option<syn::Result<()>>,
}

impl Sorted {
    fn new(input: Item) -> syn::Result<Self> {
        match input {
            Item::Enum(e) => {
                let names = e
                    .variants
                    .into_iter()
                    .map(|v| NameSpan::from(v.ident))
                    .collect();
                Ok(Self {
                    names,
                    visit_err: None,
                })
            }

            _ => Err(syn::Error::new(
                Span::call_site(),
                "expected enum or match expression",
            )),
        }
    }

    fn empty() -> Self {
        Self {
            names: vec![],
            visit_err: None,
        }
    }

    fn check_sorted(&self) -> syn::Result<()> {
        if let Some(err) = &self.visit_err {
            return err.clone();
        }
        for i in 1..self.names.len() {
            for j in 0..i {
                let pre_ident = &self.names[j];
                let cur_ident = &self.names[i];
                if pre_ident.name > cur_ident.name {
                    let msg = format!("{} should sort before {}", cur_ident, pre_ident);
                    return Err(syn::Error::new(cur_ident.span, &msg));
                }
            }
        }
        Ok(())
    }
}

#[proc_macro_attribute]
pub fn sorted(_args: TokenStream, input: TokenStream) -> TokenStream {
    let original_input = input.clone();
    let item = parse_macro_input!(input as Item);
    match Sorted::new(item) {
        Ok(sorted) => match sorted.check_sorted() {
            Ok(()) => original_input,
            Err(err) => TokenStream::from(err.to_compile_error()),
        },
        Err(err) => TokenStream::from(err.to_compile_error()),
    }
}

impl visit_mut::VisitMut for Sorted {
    fn visit_expr_match_mut(&mut self, expr_match: &mut syn::ExprMatch) {
        if expr_match.attrs[0].path.segments[0].ident.to_string() == "sorted" {
            let mut idents = vec![];
            expr_match.attrs.clear();
            for arm in &expr_match.arms {
                match &arm.pat {
                    syn::Pat::Box(_) => todo!(),
                    syn::Pat::Ident(i) => {
                        idents.push(NameSpan::from(&i.ident));
                    }
                    syn::Pat::Lit(_) => todo!(),
                    syn::Pat::Macro(_) => todo!(),
                    syn::Pat::Or(_) => todo!(),
                    syn::Pat::Path(path) => {
                        dbg!(path);
                    }
                    syn::Pat::Range(_) => todo!(),
                    syn::Pat::Reference(_) => todo!(),
                    syn::Pat::Rest(_) => todo!(),
                    syn::Pat::Slice(slice) => {
                        if self.visit_err.is_none() {
                            self.visit_err = Some(Err(syn::Error::new(
                                slice.span(),
                                "unsupported by #[sorted]",
                            )));
                        }
                    }
                    syn::Pat::Struct(_) => todo!(),
                    syn::Pat::Tuple(_) => todo!(),
                    syn::Pat::TupleStruct(tuple_struct) => {
                        let punct = tuple_struct.path.segments.clone();
                        let mut ps = vec![];
                        for p in punct.into_pairs() {
                            ps.push(p.value().ident.to_string());
                            if let Some(p) = p.punct() {
                                ps.push(p.to_token_stream().to_string());
                            }
                        }
                        idents.push(NameSpan::new(ps.join(""), tuple_struct.path.span()));
                    }
                    syn::Pat::Type(_) => todo!(),
                    syn::Pat::Verbatim(_) => todo!(),
                    syn::Pat::Wild(wild) => {
                        idents.push(NameSpan::new("_".to_string(), wild.span()));
                    }
                    _ => todo!(),
                }
            }
            self.names = idents;
        }
        visit_mut::visit_expr_match_mut(self, expr_match);
    }
}

#[proc_macro_attribute]
pub fn check(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut item_fn = parse_macro_input!(input as ItemFn);
    let mut sorted = Sorted::empty();
    sorted.visit_item_fn_mut(&mut item_fn);
    match sorted.check_sorted() {
        Ok(()) => item_fn.to_token_stream().into(),
        Err(err) => TokenStream::from_iter(vec![
            item_fn.to_token_stream().into(),
            TokenStream::from(err.to_compile_error()),
        ]),
    }
}
