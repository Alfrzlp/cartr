arma::mat DF2MAT(DataFrame &x){
    for (int i = 0; i < x.ncol(); i++){
        if(TYPEOF(x[i]) == 16){
            x[i] = internal::convert_using_rfunction(x[i], "as.integer");
        }else if(TYPEOF(x[i]) == 13 && Rf_isFactor(x[i])){
            x[i] = internal::convert_using_rfunction(x[i], "as.integer");
        }
    }
    NumericMatrix xmat = internal::convert_using_rfunction(x, "as.matrix");
    arma::mat xpred = arma::mat(xmat.begin(), xmat.nrow(), xmat.ncol(), false);
    return xpred;
}

NumericVector predictLM(NumericMatrix &bmat, DataFrame &df_x){
    arma::mat xpred = DF2MAT(df_x);
    int n = xpred.n_rows;
    arma::mat mat1 = arma::mat(n, 1, fill::ones);
    
    arma::mat X = std::move(arma::join_rows(mat1, xpred));
    arma::mat beta = arma::mat(bmat.begin(), bmat.nrow(), bmat.ncol(), false);
    arma::mat res = arma::trans(X * beta);

    return NumericVector(res.begin(), res.end());
}

List fastLM(const arma::vec & y, DataFrame & df_x, bool calculate_r2 = false) {
    arma::mat x = DF2MAT(df_x);
    int n = x.n_rows, k = x.n_cols;

    arma::mat mat1 = arma::mat(n, 1, fill::ones);
    const arma::mat X = std::move(arma::join_rows(mat1, x));
    k++;

    arma::colvec coef = arma::solve(X, y); 

    // arma::colvec resid = y - X*coef; 
    // double sig2 = arma::as_scalar(arma::trans(resid)*resid/(n-k));
    // arma::colvec stderrest = 
    //     arma::sqrt(sig2 * arma::diagvec( arma::inv(arma::trans(X)*X)) );

   if(calculate_r2){
        arma::mat J = arma::mat(n, n, fill::ones);

        double SST = arma::as_scalar(y.t() * y - (1 / double(n)) * y.t() * J * y);
        double SSR = arma::as_scalar(coef.t() * X.t() * y - (1 / double(n)) * y.t() * J * y);
        double r2 = SSR/SST;

        return List::create(
            Named("coefficients") = coef,
            Named("r2") = r2
            // Named("stderr") = stderrest
        );
   }else{
        return List::create(
            Named("coefficients") = coef
            // Named("stderr") = stderrest
        );
   }
}



// NumericVector vif(const arma::mat & x){
//     int n = x.n_cols;
//     NumericVector vif_val(n);
//     IntegerVector idx = seq(0, n - 1);

//     for (int i = 0; i < n; i++){
//         arma::uvec idsub = Rcpp::as<arma::uvec>(idx[idx != i]);
//         List res = fastLM(x.col(i), x.cols(idsub), true);   
//         vif_val(i) = 1 / (1 - double(res["r2"]));
//     }
//     return vif_val;
// }