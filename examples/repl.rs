use rcas::{
    expression::expression_tree::Expression,
    modifier::{
        adaptable_modifier::AdaptableModifier,
        default::{approximator, evaluator, simplifier},
    },
};

fn main() {
    let approx = approximator();
    let eval = evaluator();
    let simplify = simplifier();

    loop {
        println!("\nEnter an expression:");

        let mut expr_string = String::new();
        std::io::stdin().read_line(&mut expr_string).unwrap();
        let expr = &expr_string.parse::<Expression>().unwrap();

        let (eval_expr, approx_expr) = expr
            .evaluate_im::<AdaptableModifier, AdaptableModifier, AdaptableModifier, 50>(
                &approx, &eval, &simplify,
            );

        print!("\t{}", eval_expr);
        if let Some(approx) = approx_expr {
            print!(" ≈ {}", approx);
        }
    }
}
