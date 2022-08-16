use proc_macro2::{Group, Ident, Literal, TokenStream, TokenTree};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, LitInt, Token,
};

#[derive(Debug)]
struct SeqImpl {
    var: Ident,
    rep_rng: std::ops::Range<usize>,
    body: Vec<TokenTree>,
}

impl Parse for SeqImpl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let var = Ident::parse(input)?;
        let _in = input.parse::<Token![in]>()?;
        let start = LitInt::parse(input)?.base10_parse::<usize>()?;
        let _dot2 = input.parse::<Token![..]>()?;
        let include_end = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            true
        } else {
            false
        };
        let end = LitInt::parse(input)?.base10_parse::<usize>()?;
        let rep_rng = if include_end {
            start..end + 1
        } else {
            start..end
        };
        let body = TokenTree::parse(input)?;
        let body = if let TokenTree::Group(g) = body {
            g.stream().into_iter().collect()
        } else {
            panic!("not supported TokenTree")
        };
        Ok(Self { var, rep_rng, body })
    }
}

impl SeqImpl {
    fn gen_seq(&self) -> TokenStream {
        let token_trees = self.gen_seq_from_token_trees(&self.body);
        if let Some(token_trees) = token_trees {
            TokenStream::from_iter(token_trees)
        } else {
            let mut ret = vec![];
            for idx in self.rep_rng.clone() {
                let r = self.replace_var(&self.body, &self.var, idx);
                ret.extend(r);
            }
            TokenStream::from_iter(ret)
        }
    }

    fn gen_seq_from_token_trees(&self, token_trees: &Vec<TokenTree>) -> Option<Vec<TokenTree>> {
        let mut ret = vec![];
        let mut found_repetition = false;

        let mut i = 0;
        while i < token_trees.len() {
            match &token_trees[i] {
                TokenTree::Group(g) => {
                    let inner = g.stream().into_iter().collect();
                    if let Some(r) = self.gen_seq_from_token_trees(&inner) {
                        found_repetition = true;
                        let mut x =
                            TokenTree::Group(Group::new(g.delimiter(), TokenStream::from_iter(r)));
                        x.set_span(g.span());
                        ret.push(x);
                    } else {
                        ret.push(token_trees[i].clone());
                    }
                    i += 1;
                }
                TokenTree::Punct(p) if p.as_char() == '#' => {
                    match (token_trees.get(i + 1), token_trees.get(i + 2)) {
                        (Some(TokenTree::Group(g)), Some(TokenTree::Punct(t)))
                            if t.as_char() == '*'
                                && g.delimiter() == proc_macro2::Delimiter::Parenthesis =>
                        {
                            found_repetition = true;
                            let inner = g.stream().into_iter().collect();
                            let x = self
                                .gen_seq_from_token_trees(&inner)
                                .unwrap_or_else(|| inner.clone());
                            for idx in self.rep_rng.clone() {
                                let r = self.replace_var(&x, &self.var, idx);
                                ret.extend(r);
                            }
                            i += 3;
                        }
                        _ => {
                            ret.push(token_trees[i].clone());
                            i += 1;
                        }
                    }
                }
                _ => {
                    ret.push(token_trees[i].clone());
                    i += 1;
                }
            }
        }

        if found_repetition {
            Some(ret)
        } else {
            None
        }
    }

    fn replace_var(
        &self,
        token_trees: &Vec<TokenTree>,
        search_var: &Ident,
        target_idx: usize,
    ) -> Vec<TokenTree> {
        let mut ret = vec![];
        let mut i = 0;
        while i < token_trees.len() {
            let token_tree = token_trees[i].clone();
            if let TokenTree::Ident(ident) = &token_tree {
                match (token_trees.get(i + 1), token_trees.get(i + 2)) {
                    (Some(TokenTree::Punct(p)), Some(TokenTree::Ident(v)))
                        if p.as_char() == '~' && v.to_string() == search_var.to_string() =>
                    {
                        let var = Ident::new(
                            &format!("{}{}", ident.to_string(), target_idx),
                            ident.span(),
                        );
                        ret.push(TokenTree::Ident(var));
                        i += 3;
                    }
                    _ => {
                        ret.push(self.replace_token_tree(token_tree, search_var, target_idx));
                        i += 1;
                    }
                }
            } else {
                ret.push(self.replace_token_tree(token_tree, search_var, target_idx));
                i += 1;
            }
        }
        ret
    }

    fn replace_token_tree(
        &self,
        token_tree: TokenTree,
        search_var: &Ident,
        target_idx: usize,
    ) -> TokenTree {
        let ret = match &token_tree {
            TokenTree::Group(group) => {
                let sp = group.span();
                let delimiter = group.delimiter();
                let result = self.replace_var(
                    &group.stream().into_iter().collect(),
                    search_var,
                    target_idx,
                );
                let mut x = TokenTree::Group(Group::new(delimiter, TokenStream::from_iter(result)));
                x.set_span(sp);
                x
            }
            TokenTree::Ident(ident) => {
                if ident.to_string() == search_var.to_string() {
                    let mut lit = Literal::usize_unsuffixed(target_idx);
                    lit.set_span(ident.span());
                    TokenTree::Literal(lit)
                } else {
                    token_tree
                }
            }
            _ => token_tree,
        };
        ret
    }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let seq_impl = parse_macro_input!(input as SeqImpl);
    seq_impl.gen_seq().into()
}
