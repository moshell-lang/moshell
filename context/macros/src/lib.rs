use proc_macro::TokenStream;
use quote::quote;
use syn::parse::Parser;
use syn::{parse, parse_macro_input, ItemStruct};

#[proc_macro_attribute]
pub fn segment_holder(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut item_struct = parse_macro_input!(input as ItemStruct);
    let _ = parse_macro_input!(args as parse::Nothing);

    if let syn::Fields::Named(ref mut fields) = item_struct.fields {
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! { pub segment: context::source::SourceSegment })
                .unwrap(),
        );
    }

    let generics = &item_struct.generics;
    let type_name = &item_struct.ident;
    let where_clause = &item_struct.generics.where_clause;

    return quote! {
        #item_struct

        impl #generics context::source::SourceSegmentHolder for #type_name #generics #where_clause {
            fn segment(&self) -> context::source::SourceSegment {
                self.segment.clone()
            }
        }
    }
    .into();
}
