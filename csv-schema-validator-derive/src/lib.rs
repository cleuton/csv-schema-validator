// csv-schema-validator-derive/src/lib.rs
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, GenericArgument, Ident,
    PathArguments, Type,
};

// Armazena validações por campo
struct FieldValidation {
    field_name: Ident,
    is_option: bool, // [FIX] passamos a carregar se o campo é Option<T>
    validations: Vec<Validation>,
    core_ty_ts: proc_macro2::TokenStream, // v.0.1.3: tipo efetivo (T de Option<T> ou o próprio) para codegen
}

// Tipos de validações suportadas
enum Validation {
    Range { min: Option<String>, max: Option<String>, is_float: bool }, // v.0.1.3
    Regex { regex: String },
    Required,
    Custom { path: syn::Path },
    Length { min: usize, max: usize },
    NotBlank,
    OneOf { values: Vec<String> },
    NotIn { values: Vec<String> },
    IfThen { conditional_column: String, conditional_value: String, expected_value: String }, // v 0.2.0 
}

impl Validation {
    /// Faz o parse de #[validate(...)] em uma lista de Validation
    fn parse_validations(input: syn::parse::ParseStream) -> syn::Result<Vec<Self>> {
        let mut out = Vec::new();
        let meta_items = syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated(input)?;
        for meta in meta_items {
            match meta {
                syn::Meta::Path(path) => Self::parse_meta_path(path, &mut out)?,
                syn::Meta::NameValue(mnv) => Self::parse_meta_name_value(mnv, &mut out)?,
                syn::Meta::List(list) => Self::parse_meta_list(list, &mut out)?,
            }
        }
        Ok(out)
    }

    // ---------- Dispatchers por variante de Meta ----------

    fn parse_meta_path(path: syn::Path, out: &mut Vec<Self>) -> syn::Result<()> {
        if path.is_ident("required") {
            out.push(Validation::Required);
        } else if path.is_ident("not_blank") {
            out.push(Validation::NotBlank);
        }
        Ok(())
    }

    fn parse_meta_name_value(mnv: syn::MetaNameValue, out: &mut Vec<Self>) -> syn::Result<()> {
        if mnv.path.is_ident("regex") {
            let s = Self::expect_lit_str(&mnv.value, "Expected string literal for `regex`")?;
            out.push(Validation::Regex { regex: s });
            Ok(())
        } else if mnv.path.is_ident("custom") {
            let s = Self::expect_lit_str(&mnv.value, "Expected string literal for `custom` (e.g., custom = \"path::to::fn\")")?;
            let path: syn::Path = syn::parse_str(&s).map_err(|e| syn::Error::new_spanned(&mnv.value, e))?;
            out.push(Validation::Custom { path });
            Ok(())
        } else {
            Err(syn::Error::new_spanned(mnv, "chave desconhecida em atributo"))
        }
    }

    fn parse_meta_list(list: syn::MetaList, out: &mut Vec<Self>) -> syn::Result<()> {
        let ident = &list.path;
        if ident.is_ident("length") {
            Self::parse_length_list(list, out)
        } else if ident.is_ident("range") {
            Self::parse_range_list(list, out)
        } else if ident.is_ident("one_of") {
            Self::parse_one_of_list(list, out)
        } else if ident.is_ident("not_in") {
            Self::parse_not_in_list(list, out)
        } else if ident.is_ident("if_then") {
            Self::parse_if_then_list(list, out)
        } else {
            Ok(())
        }
    }

    // ---------- Handlers específicos ----------

    fn parse_if_then_list(list: syn::MetaList, out: &mut Vec<Self>) -> syn::Result<()> {
        use syn::{LitStr, Token};
        use syn::punctuated::Punctuated;

        // Espera exatamente 3 literais string
        let args = list.parse_args_with(Punctuated::<LitStr, Token![,]>::parse_terminated)?;
        if args.len() != 3 {
            return Err(syn::Error::new_spanned(
                list,
                "if_then espera exatamente 3 strings: (conditional_column, conditional_value, expected_value)",
            ));
        }

        let conditional_column = args[0].value();
        let conditional_value  = args[1].value();
        let expected_value     = args[2].value();

        out.push(Self::IfThen { conditional_column, conditional_value, expected_value });
        Ok(())
    }

    fn parse_length_list(list: syn::MetaList, out: &mut Vec<Self>) -> syn::Result<()> {
        let items: syn::punctuated::Punctuated<syn::MetaNameValue, syn::Token![,]> =
            list.parse_args_with(syn::punctuated::Punctuated::parse_terminated)?;
        let mut min: Option<usize> = None;
        let mut max: Option<usize> = None;

        for kv in items {
            if kv.path.is_ident("min") {
                let v = Self::expect_lit_int(&kv.value, "`min` for `length` must be an integer literal")?;
                min = Some(v);
            } else if kv.path.is_ident("max") {
                let v = Self::expect_lit_int(&kv.value, "`max` for `length` must be an integer literal")?;
                max = Some(v);
            }
        }

        if min.is_none() && max.is_none() {
            return Err(syn::Error::new_spanned(list, "`length` requires at least one of `min` or `max`"));
        }
        if let Some(mx) = max {
            if mx == 0 {
                return Err(syn::Error::new_spanned(list, "`max` for `length` cannot be zero"));
            }
        }
        if let (Some(a), Some(b)) = (min, max) {
            if a > b {
                return Err(syn::Error::new_spanned(list, "`min` must be <= `max` for `length`"));
            }
        }

        out.push(Validation::Length {
            min: min.unwrap_or(0),
            max: max.unwrap_or(usize::MAX),
        });
        Ok(())
    }

    fn parse_range_list(list: syn::MetaList, out: &mut Vec<Self>) -> syn::Result<()> {
        let items: syn::punctuated::Punctuated<syn::MetaNameValue, syn::Token![,]> =
            list.parse_args_with(syn::punctuated::Punctuated::parse_terminated)?;
        let mut min: Option<String> = None;
        let mut max: Option<String> = None;
        let mut min_is_float = false;
        let mut max_is_float = false;

        for kv in items {
            let (slot_val, slot_is_float) = if kv.path.is_ident("min") {
                (&mut min, &mut min_is_float)
            } else if kv.path.is_ident("max") {
                (&mut max, &mut max_is_float)
            } else {
                continue;
            };

            match &kv.value {
                // inteiros positivos
                syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Int(i), .. }) => {
                    *slot_val = Some(i.to_string());
                    *slot_is_float = false;
                }
                // floats positivos
                syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Float(f), .. }) => {
                    *slot_val = Some(f.to_string());
                    *slot_is_float = true;
                }
                // negativos: -<int> ou -<float>
                syn::Expr::Unary(syn::ExprUnary { op: syn::UnOp::Neg(_), expr, .. }) => {
                    match &**expr {
                        syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Int(i), .. }) => {
                            *slot_val = Some(format!("-{}", i.to_string()));
                            *slot_is_float = false;
                        }
                        syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Float(f), .. }) => {
                            *slot_val = Some(format!("-{}", f.to_string()));
                            *slot_is_float = true;
                        }
                        _ => {
                            return Err(syn::Error::new_spanned(
                                &kv.value,
                                "`range` values must be numeric literals (int or float)",
                            ));
                        }
                    }
                }
                _ => {
                    return Err(syn::Error::new_spanned(
                        &kv.value,
                        "`range` values must be numeric literals (int or float)",
                    ));
                }
            }
        }

        if min.is_none() && max.is_none() {
            return Err(syn::Error::new_spanned(
                &list,
                "`range` requires at least one of `min` or `max`",
            ));
        }

        // Se ambos existem, precisam ser do mesmo "kind" (ambos int ou ambos float)
        if min.is_some() && max.is_some() && (min_is_float != max_is_float) {
            return Err(syn::Error::new_spanned(
                &list,
                "`range` `min` and `max` must be of the same type (both int or both float)",
            ));
        }

        // Checagem min <= max quando ambos existem
        if let (Some(ref a), Some(ref b)) = (&min, &max) {
            if min_is_float {
                // float
                let av: f64 = a.parse().map_err(|_| syn::Error::new_spanned(&list, "`range` float literal parse error"))?;
                let bv: f64 = b.parse().map_err(|_| syn::Error::new_spanned(&list, "`range` float literal parse error"))?;
                if av > bv {
                    return Err(syn::Error::new_spanned(&list, "`range` `min` must be <= `max`"));
                }
            } else {
                // inteiro (suporta sinal)
                let av: i128 = a.parse().map_err(|_| syn::Error::new_spanned(&list, "`range` int literal parse error"))?;
                let bv: i128 = b.parse().map_err(|_| syn::Error::new_spanned(&list, "`range` int literal parse error"))?;
                if av > bv {
                    return Err(syn::Error::new_spanned(&list, "`range` `min` must be <= `max`"));
                }
            }
        }

        let is_float = min_is_float || max_is_float;

        out.push(Validation::Range { min, max, is_float });
        Ok(())
    }

    fn parse_one_of_list(list: syn::MetaList, out: &mut Vec<Self>) -> syn::Result<()> {
        let exprs: syn::punctuated::Punctuated<syn::Expr, syn::Token![,]> =
            list.parse_args_with(syn::punctuated::Punctuated::parse_terminated)?;
        let mut values = Vec::new();
        for expr in exprs {
            let s = Self::expect_lit_str_expr(expr, "`one_of` only accepts string literals")?;
            values.push(s);
        }
        if values.is_empty() {
            return Err(syn::Error::new_spanned(list, "`one_of` requires at least one value"));
        }
        out.push(Validation::OneOf { values });
        Ok(())
    }

    fn parse_not_in_list(list: syn::MetaList, out: &mut Vec<Self>) -> syn::Result<()> {
        let exprs: syn::punctuated::Punctuated<syn::Expr, syn::Token![,]> =
            list.parse_args_with(syn::punctuated::Punctuated::parse_terminated)?;
        let mut values = Vec::new();
        for expr in exprs {
            let s = Self::expect_lit_str_expr(expr, "`not_in` only accepts string literals")?;
            values.push(s);
        }
        if values.is_empty() {
            return Err(syn::Error::new_spanned(list, "`not_in` requires at least one value"));
        }
        out.push(Validation::NotIn { values });
        Ok(())
    }

    // ---------- UTILITÁRIOS GERAIS ----------

    fn type_name_of(ty: &Type) -> String {
        if let Type::Path(tp) = ty {
            tp.path.segments.last().map(|s| s.ident.to_string()).unwrap_or_default()
        } else { String::new() }
    }

    fn is_int_ty(n: &str) -> bool {
        matches!(n, "i8"|"i16"|"i32"|"i64"|"i128"|"isize"|
                    "u8"|"u16"|"u32"|"u64"|"u128"|"usize")
    }

    fn is_float_ty(n: &str) -> bool {
        matches!(n, "f32"|"f64")
    }

    // ---------- FUNÇÕES AUXILIARES DE VALIDAÇÃO ----------

    fn validate_if_then_for_field(
        v: &[Validation],
        field: &syn::Field,
        field_name: &syn::Ident,
        is_option: bool,
        fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
    ) -> Result<(), proc_macro::TokenStream> {
        // Procura um if_then neste campo (se houver vários, valida o primeiro; repita se quiser todos)
        let Some((conditional_column, conditional_value, expected_value)) =
            v.iter().find_map(|vv| {
                if let Validation::IfThen { conditional_column, conditional_value, expected_value } = vv {
                    Some((conditional_column.as_str(), conditional_value.as_str(), expected_value.as_str()))
                } else {
                    None
                }
            })
        else {
            return Ok(());
        };
        

        // 1) Campo alvo deve ser Option<_>
        if !is_option {
            return Err(
                syn::Error::new_spanned(
                    &field.ty,
                    format!("`if_then` só pode ser usado em campos Option<T> (campo `{}`)", field_name)
                ).to_compile_error().into()
            );
        }

        // 2) Coluna condicional: existe e é Option<_>
        let Some(cond_field) = fields.iter().find(|f|
            f.ident.as_ref().map(|i| i.to_string() == conditional_column).unwrap_or(false)        
        ) else {
            return Err(
                syn::Error::new_spanned(
                    &field.ty,
                    format!("`if_then`: campo condicional `{}` não existe na struct", conditional_column)
                ).to_compile_error().into()
            );
        };

        let Some(cond_core_ty) = option_inner_type(&cond_field.ty) else {
            return Err(
                syn::Error::new_spanned(
                    &cond_field.ty,
                    format!("`if_then`: campo condicional `{}` deve ser Option<U>", conditional_column)
                ).to_compile_error().into()
            );
        };

        // Tipos base
        let cond_ty_name   = Self::type_name_of(cond_core_ty);                          // T
        let Some(target_core_ty) = option_inner_type(&field.ty) else { unreachable!() }; // R
        let target_ty_name = Self::type_name_of(target_core_ty);                         // R

        // 3) Compatibilidade de literais (checagem leve)
        if cond_ty_name != "String" {
            if Self::is_int_ty(&cond_ty_name) {
                if conditional_value.parse::<i128>().is_err() && conditional_value.parse::<u128>().is_err() {
                    return Err(
                        syn::Error::new_spanned(
                            &field.ty,
                            format!("`if_then`: `conditional_value`='{}' inválido para tipo {}", conditional_value, cond_ty_name)
                        ).to_compile_error().into()
                    );
                }
            } else if Self::is_float_ty(&cond_ty_name) {
                if conditional_value.parse::<f64>().is_err() {
                    return Err(
                        syn::Error::new_spanned(
                            &field.ty,
                            format!("`if_then`: `conditional_value`='{}' inválido para tipo {}", conditional_value, cond_ty_name)
                        ).to_compile_error().into()
                    );
                }
            } else {
                return Err(
                    syn::Error::new_spanned(
                        &field.ty,
                        format!("`if_then`: tipo condicional `{}` não suportado; use String ou numérico", cond_ty_name)
                    ).to_compile_error().into()
                );
            }
        }

        if target_ty_name != "String" {
            if Self::is_int_ty(&target_ty_name) {
                if expected_value.parse::<i128>().is_err() && expected_value.parse::<u128>().is_err() {
                    return Err(
                        syn::Error::new_spanned(
                            &field.ty,
                            format!("`if_then`: `expected_value`='{}' inválido para tipo {}", expected_value, target_ty_name)
                        ).to_compile_error().into()
                    );
                }
            } else if Self::is_float_ty(&target_ty_name) {
                if expected_value.parse::<f64>().is_err() {
                    return Err(
                        syn::Error::new_spanned(
                            &field.ty,
                            format!("`if_then`: `expected_value`='{}' inválido para tipo {}", expected_value, target_ty_name)
                        ).to_compile_error().into()
                    );
                }
            } else {
                return Err(
                    syn::Error::new_spanned(
                        &field.ty,
                        format!("`if_then`: tipo do campo `{}` não suportado; use String ou numérico", target_ty_name)
                    ).to_compile_error().into()
                );
            }
        }

        Ok(())
    }


    // ---------- Utilitários de extração ----------

    fn expect_lit_str(expr: &syn::Expr, msg: &str) -> syn::Result<String> {
        if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(s), .. }) = expr {
            Ok(s.value())
        } else {
            Err(syn::Error::new_spanned(expr, msg))
        }
    }

    fn expect_lit_int(expr: &syn::Expr, msg: &str) -> syn::Result<usize> {
        if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Int(i), .. }) = expr {
            i.base10_parse::<usize>().map_err(|e| syn::Error::new_spanned(expr, e))
        } else {
            Err(syn::Error::new_spanned(expr, msg))
        }
    }

    fn expect_lit_str_expr(expr: syn::Expr, msg: &str) -> syn::Result<String> {
        if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(s), .. }) = expr {
            Ok(s.value())
        } else {
            Err(syn::Error::new_spanned(expr, msg))
        }
    }
}


// [FIX] helper para detectar Option<T>
fn option_inner_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(tp) = ty {
        if let Some(seg) = tp.path.segments.last() {
            if seg.ident == "Option" {
                if let PathArguments::AngleBracketed(args) = &seg.arguments {
                    if let Some(GenericArgument::Type(inner_ty)) = args.args.first() {
                        return Some(inner_ty);
                    }
                }
            }
        }
    }
    None
}

#[proc_macro_derive(ValidateCsv, attributes(validate))]
pub fn validate_csv_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(f) => &f.named,
            _ => {
                return syn::Error::new_spanned(
                    &data.fields,
                    "only structs with named fields are supported",
                )
                .to_compile_error()
                .into();
            }
        },
        _ => {
            return syn::Error::new_spanned(&input, "only structs are supported")
                .to_compile_error()
                .into();
        }
    };

    let mut field_validations = Vec::new();

    for field in fields {
        let field_name = field.ident.as_ref().unwrap().clone();
        let is_option = option_inner_type(&field.ty).is_some(); // [FIX] capturamos se é Option<T>
        let mut validations = Vec::new();

        for attr in &field.attrs {
            if attr.path().is_ident("validate") {
                match attr.parse_args_with(Validation::parse_validations) {
                    Ok(mut v) => {
                        // v.0.1.3: `required` só em Option<T>
                        let has_required = v.iter().any(|vv| matches!(vv, Validation::Required)); // v.0.1.3
                        if has_required && !is_option {                                          // v.0.1.3
                            return syn::Error::new_spanned(
                                &field.ty,
                                format!("`required` can only be used on Option<T> fields (field `{}`)", field_name)
                            ).to_compile_error().into();
                        }

                        // v.0.1.3: Restrições de tipo para validações baseadas em String
                        let needs_string = v.iter().any(|vv| matches!(
                            vv,
                            Validation::Regex{..} | Validation::Length{..} | Validation::NotBlank
                            | Validation::OneOf{..} | Validation::NotIn{..}
                        )); // v.0.1.3
                        if needs_string {
                            let core_ty = option_inner_type(&field.ty).unwrap_or(&field.ty); // v.0.1.3
                            let ty_name = Validation::type_name_of(core_ty);                                  // v.0.1.3
                            if ty_name != "String" {                                         // v.0.1.3
                                return syn::Error::new_spanned(
                                    core_ty,
                                    format!("`regex`, `length`, `not_blank`, `one_of`, `not_in` require String (field `{}` is `{}`)", field_name, ty_name)
                                ).to_compile_error().into();                                 // v.0.1.3
                            }
                        }

                        // v.0.1.3: Restrições de tipo para `range`
                        if let Some(is_float) = v.iter().find_map(|vv| {
                            if let Validation::Range{is_float, ..} = vv { Some(*is_float) } else { None }
                        }) { // v.0.1.3
                            let core_ty = option_inner_type(&field.ty).unwrap_or(&field.ty); // v.0.1.3
                            let ty_name     = Validation::type_name_of(core_ty);
                            let is_int      = Validation::is_int_ty(&ty_name);
                            let is_float_ty = Validation::is_float_ty(&ty_name);
                            if !(is_int || is_float_ty) {
                                return syn::Error::new_spanned(
                                    core_ty,
                                    format!("`range` only applies to numeric fields (field `{}` is `{}`)", field_name, ty_name)
                                ).to_compile_error().into(); // v.0.1.3
                            }
                            if is_float && !is_float_ty {
                                return syn::Error::new_spanned(
                                    core_ty,
                                    format!("`range` with float literals requires float field (field `{}` is `{}`)", field_name, ty_name)
                                ).to_compile_error().into(); // v.0.1.3
                            }
                            if !is_float && !is_int {
                                return syn::Error::new_spanned(
                                    core_ty,
                                    format!("`range` with integer literals requires integer field (field `{}` is `{}`)", field_name, ty_name)
                                ).to_compile_error().into(); // v.0.1.3
                            }
                        }

                        // valida if_then para este campo (se houver)
                        if let Err(ts) = Validation::validate_if_then_for_field(&v, field, &field_name, is_option, fields) {
                            return ts; // já vem com to_compile_error().into()
                        }

                        validations.append(&mut v);
                    },
                    Err(e) => return e.to_compile_error().into(),
                }
            }
        }

        if !validations.is_empty() {
            // v.0.1.3: calcular e guardar tipo efetivo para codegen (T se Option<T>, senão o próprio)
            let core_ty = option_inner_type(&field.ty).unwrap_or(&field.ty); // v.0.1.3
            let core_ty_ts = quote! { #core_ty };                            // v.0.1.3

            field_validations.push(FieldValidation {
                field_name,
                is_option,
                validations,
                core_ty_ts,   // v.0.1.3
            });
        }
    }

    let validation_arms = field_validations.into_iter().map(|fv| {
        let field_name_str = fv.field_name.to_string();
        let field_name_ident = fv.field_name;
        let fv_is_option = fv.is_option;
        let fv_core_ty_ts = fv.core_ty_ts.clone(); // v.0.1.3
    
        let checks = fv.validations.into_iter().map(|validation| {
            match validation {
                Validation::Required => {
                    gen_required_check(&field_name_ident, &field_name_str)
                }
                Validation::NotBlank => {
                    gen_not_blank_check(&field_name_ident, &field_name_str, fv_is_option)
                }
                Validation::Range { min, max, is_float: _ } => { // v.0.1.3
                    gen_range_check(&field_name_ident, &field_name_str, fv_is_option, min, max, fv_core_ty_ts.clone()) // v.0.1.3
                }
                Validation::Length { min, max } => {
                    gen_length_check(&field_name_ident, &field_name_str, fv_is_option, min, max)
                }
                Validation::Regex { regex } => {
                    gen_regex_check(&field_name_ident, &field_name_str, fv_is_option, regex)
                }
                Validation::OneOf { values } => {
                    gen_one_of_check(&field_name_ident, &field_name_str, fv_is_option, values)
                }
                Validation::NotIn { values } => {
                    gen_not_in_check(&field_name_ident, &field_name_str, fv_is_option, values)
                }
                Validation::Custom { path } => {
                    gen_custom_check(&field_name_ident, &field_name_str, fv_is_option, path)
                }
                Validation::IfThen { conditional_column, conditional_value, expected_value } => proc_macro2::TokenStream::new(), // v 0.2.0
            }
        });
    
        quote! { #(#checks)* }
    });

    let expanded = quote! {
        impl #name {
            pub fn validate_csv(&self) -> ::core::result::Result<(), ::std::vec::Vec<::csv_schema_validator::ValidationError>> {
                let mut errors = ::std::vec::Vec::new();
                #(#validation_arms)*
                if errors.is_empty() {
                    Ok(())
                } else {
                    Err(errors)
                }
            }
        }
    };

    TokenStream::from(expanded)
}

use proc_macro2::TokenStream as TokenStream2;

// Helpers por regra
fn gen_required_check(field_ident: &syn::Ident, field_name: &str) -> TokenStream2 {
    quote! {
        if (&self.#field_ident).is_none() {
            errors.push(::csv_schema_validator::ValidationError {
                field: #field_name.to_string(),
                message: "mandatory field".to_string(),
            });
        }
    }
}

fn gen_not_blank_check(field_ident: &syn::Ident, field_name: &str, is_option: bool) -> TokenStream2 {
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                if value.trim().is_empty() {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: "must not be blank or contain only whitespace".to_string(),
                    });
                }
            }
        }
    } else {
        quote! {
            let value = &self.#field_ident;
            if value.trim().is_empty() {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: "must not be blank or contain only whitespace".to_string(),
                });
            }
        }
    }
}

// v.0.1.3: `range` agora recebe min/max como Option<String> e usa o tipo efetivo do campo;
// tipamos os literais com o tipo do campo, para o compilador checar compatibilidade/overflow.
// Também produzimos mensagens específicas quando ambos os limites existem.
fn gen_range_check(
    field_ident: &syn::Ident,
    field_name: &str,
    is_option: bool,
    min: Option<String>,         // v.0.1.3
    max: Option<String>,         // v.0.1.3
    core_ty_ts: proc_macro2::TokenStream, // v.0.1.3
) -> TokenStream2 {
    let min_ts = min.as_ref().map(|s| syn::parse_str::<proc_macro2::TokenStream>(s).expect("invalid min literal")); // v.0.1.3
    let max_ts = max.as_ref().map(|s| syn::parse_str::<proc_macro2::TokenStream>(s).expect("invalid max literal")); // v.0.1.3

    let min_bind = min_ts.as_ref().map(|ts| quote! { let __csv_min: #core_ty_ts = #ts; }); // v.0.1.3
    let max_bind = max_ts.as_ref().map(|ts| quote! { let __csv_max: #core_ty_ts = #ts; }); // v.0.1.3

    // v.0.1.3: mensagens amigáveis (normalizamos sufixo ".0")
    fn normalize_for_msg(s: &str) -> String { // v.0.1.3
        if let Some(stripped) = s.strip_suffix(".0") { stripped.to_string() } else { s.to_string() }
    }
    let msg_between = match (min.as_ref(), max.as_ref()) { // v.0.1.3
        (Some(a), Some(b)) => format!("value out of expected range: {} to {}", normalize_for_msg(a), normalize_for_msg(b)),
        _ => "value out of expected range".to_string(),
    };
    let msg_below = match min.as_ref() { // v.0.1.3
        Some(a) => format!("value below min: {}", normalize_for_msg(a)),
        None => "value below min".to_string(),
    };
    let msg_above = match max.as_ref() { // v.0.1.3
        Some(b) => format!("value above max: {}", normalize_for_msg(b)),
        None => "value above max".to_string(),
    };

    let cmp = match (min_bind.is_some(), max_bind.is_some()) {
        (true, true) => quote! {
            if !(__csv_min <= *value && *value <= __csv_max) {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: #msg_between.to_string(), // v.0.1.3
                });
            }
        },
        (true, false) => quote! {
            if !(__csv_min <= *value) {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: #msg_below.to_string(), // v.0.1.3
                });
            }
        },
        (false, true) => quote! {
            if !(*value <= __csv_max) {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: #msg_above.to_string(), // v.0.1.3
                });
            }
        },
        _ => quote! {}, // impossível pois já validamos no parse
    };

    if is_option {
        quote! {
            { #min_bind #max_bind
              if let Some(value) = &self.#field_ident { #cmp } }
        }
    } else {
        quote! {
            { #min_bind #max_bind
              let value = &self.#field_ident; #cmp }
        }
    }
}

fn gen_length_check(field_ident: &syn::Ident, field_name: &str, is_option: bool, min: usize, max: usize) -> TokenStream2 {
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                let len = value.len();
                if len < #min || len > #max {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: format!("length out of expected range: {} to {}", #min, #max),
                    });
                }
            }
        }
    } else {
        quote! {
            let value = &self.#field_ident;
            let len = value.len();
            if len < #min || len > #max {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: format!("length out of expected range: {} to {}", #min, #max),
                });
            }
        }
    }
}

fn gen_regex_check(field_ident: &syn::Ident, field_name: &str, is_option: bool, regex: String) -> TokenStream2 {
    let body = quote! {
        use ::csv_schema_validator::__private::once_cell::sync::Lazy;
        use ::csv_schema_validator::__private::regex;
        static RE: Lazy<Result<regex::Regex, regex::Error>> = Lazy::new(|| regex::Regex::new(#regex));

        match RE.as_ref() {
            Ok(compiled_regex) => {
                if !compiled_regex.is_match(value) {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: "does not match the expected pattern".to_string(),
                    });
                }
            }
            Err(e) => {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: format!("invalid regex '{}': {}", #regex, e),
                });
            }
        }
    };
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                #body
            }
        }
    } else {
        quote! {
            let value = &self.#field_ident;
            #body
        }
    }
}

fn gen_one_of_check(field_ident: &syn::Ident, field_name: &str, is_option: bool, values: Vec<String>) -> TokenStream2 {
    let arr = values; // mantém o padrão já usado
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                const __ALLOWED: &[&str] = &[#(#arr),*];
                if !__ALLOWED.contains(&value.as_str()) {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: format!("invalid value"),
                    });
                }
            }
        }
    } else {
        quote! {
            let value = &self.#field_ident;
            const __ALLOWED: &[&str] = &[#(#arr),*];
            if !__ALLOWED.contains(&value.as_str()) {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: format!("invalid value"),
                });
            }
        }
    }
}

fn gen_not_in_check(field_ident: &syn::Ident, field_name: &str, is_option: bool, values: Vec<String>) -> TokenStream2 {
    let arr = values;
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                const __FORBIDDEN: &[&str] = &[#(#arr),*];
                if __FORBIDDEN.contains(&value.as_str()) {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: format!("value not allowed"),
                    });
                }
            }
        }
    } else {
        quote! {
            let value = &self.#field_ident;
            const __FORBIDDEN: &[&str] = &[#(#arr),*];
            if __FORBIDDEN.contains(&value.as_str()) {
                errors.push(::csv_schema_validator::ValidationError {
                    field: #field_name.to_string(),
                    message: format!("value not allowed"),
                });
            }
        }
    }
}

fn gen_custom_check(field_ident: &syn::Ident, field_name: &str, is_option: bool, path: syn::Path) -> TokenStream2 {
    if is_option {
        quote! {
            if let Some(value) = &self.#field_ident {
                match #path(value) {
                    Err(err) => {
                        errors.push(::csv_schema_validator::ValidationError {
                            field: #field_name.to_string(),
                            message: format!("{}", err),
                        });
                    }
                    Ok(()) => {}
                }
            }
        }
    } else {
        quote! {
            match #path(&self.#field_ident) {
                Err(err) => {
                    errors.push(::csv_schema_validator::ValidationError {
                        field: #field_name.to_string(),
                        message: format!("{}", err),
                    });
                }
                Ok(()) => {}
            }
        }
    }
}
