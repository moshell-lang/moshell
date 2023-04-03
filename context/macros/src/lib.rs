use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(SourceSegmentHolder)]
pub fn derive_src_segment_holder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let generics = &input.generics;
    let type_name = &input.ident;
    let where_clause = &input.generics.where_clause;

    let expanded = quote! {
        impl #generics context::source::SourceSegmentHolder for #type_name #generics #where_clause {
            fn segment(&self) -> context::source::SourceSegment {
                self.segment.clone()
            }
        }
    };
    TokenStream::from(expanded)
}
