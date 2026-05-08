#[cfg(test)]
mod tests {
    use righton::ast::*;
    use righton::compiler::LLVMTextGen;

    #[test]
    fn test_infer_expr_type_for_float() {
        let generator = LLVMTextGen::new();
        let expr = Expr::Literal(Literal::Float(3.14));
        let params = vec![];
        let locals = std::collections::HashMap::new();

        let result = generator.infer_expr_type(&expr, &params, &locals);
        assert_eq!(result, "double");
    }

    #[test]
    fn test_infer_expr_type_for_boolean() {
        let generator = LLVMTextGen::new();
        let expr = Expr::Literal(Literal::Bool(true));
        let params = vec![];
        let locals = std::collections::HashMap::new();

        let result = generator.infer_expr_type(&expr, &params, &locals);
        assert_eq!(result, "i32");
    }

    #[test]
    fn test_infer_expr_type_for_integer() {
        let generator = LLVMTextGen::new();
        let expr = Expr::Literal(Literal::Int(42));
        let params = vec![];
        let locals = std::collections::HashMap::new();

        let result = generator.infer_expr_type(&expr, &params, &locals);
        assert_eq!(result, "i32");
    }

    #[test]
    fn test_binary_operation_type_inference() {
        let generator = LLVMTextGen::new();
        let expr = Expr::Binary(
            Box::new(Expr::Literal(Literal::Float(2.5))),
            BinOp::Add,
            Box::new(Expr::Literal(Literal::Float(3.5))),
        );
        let params = vec![];
        let locals = std::collections::HashMap::new();

        let result = generator.infer_expr_type(&expr, &params, &locals);
        assert_eq!(result, "double");
    }
}