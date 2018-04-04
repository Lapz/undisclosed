 use std::collections::HashSet;
 use syntax::ast::{Ident,Program,TyAlias};
 use util::pos::{Span,Spanned};

pub struct Resolver {
    values:Vec<HashSet<(Ident,Span)>>
}


impl Resolver {
    fn begin_scope(&mut self) {
        self.values.push(HashSet::new())
    }

    fn end_scope(&mut self) {
        self.values.pop();
    }

    fn resolve_alias(alias:&Spanned<TyAlias>) {
        
    }
}
