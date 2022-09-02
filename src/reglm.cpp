#include <string>
#include <algorithm>
#include <RcppArmadillo.h>

// [[Rcpp::plugins(cpp17)]]  
// [[Rcpp::depends(RcppArmadillo)]]

using namespace std;
using namespace Rcpp;
using namespace arma;

#include "utils.h"
#include "linearModel.h"

//  Integer Value SEXPTYPE  R Vector     Rcpp Vector
//             10   LGLSXP   logical   LogicalVector
//             13   INTSXP   integer   IntegerVector
//             14  REALSXP   numeric   NumericVector
//             15  CPLXSXP   complex   ComplexVector
//             16   STRSXP character CharacterVector

CharacterVector COL_NAMES;
NumericVector FI, PREDICTION;
string METHOD;
int N, MAX_DEPTH, MIN_SAMPLES_LEAF, MIN_SAMPLES_SPLIT;
double MIN_IG;


Rcpp::List getBestSplitCat(const NumericVector &y, const CharacterVector &x) {
    CharacterVector unique_x = unique(x);
    LogicalVector mask;
    int s1, s2, sz = y.size();
    double ig, best_ig = -1, vary = Var(y);
    string best_split;
        
    for (int i = 0; i < unique_x.length(); i++){
        mask = get_mask(x, Rcpp::as<std::string>(unique_x[i]));
        
        s1 = sum(mask);
        s2 = sz - s1;

        if(s1 < MIN_SAMPLES_LEAF || s2 < MIN_SAMPLES_LEAF){
            continue;
        }

        ig = vary - (double(s1)*Var(y[mask]) + double(s2)*Var(y[!mask]))/double(sz);
        if(ig > best_ig){
            best_ig = ig;
            best_split = unique_x[i];
        }
    }

    return Rcpp::List::create(
        Rcpp::Named("best_ig") = best_ig,
        Rcpp::Named("best_split") = best_split
    );
}

Rcpp::List getBestSplitFct(const NumericVector &y, const IntegerVector &x) {
    IntegerVector unique_x = unique(x);
    LogicalVector mask;
    int s1, s2, sz = y.size();
    double ig, best_ig = -1, vary = Var(y);
    int best_split = 0;
        
    for (int i = 0; i < unique_x.length(); i++){
        mask = x == unique_x[i];

        s1 = sum(mask);
        s2 = sz - s1;

        if(s1 < MIN_SAMPLES_LEAF || s2 < MIN_SAMPLES_LEAF){
            continue;
        }

        ig = vary - (double(s1)*Var(y[mask]) + double(s2)*Var(y[!mask]))/double(sz);
        if(ig > best_ig){
            best_ig = ig;
            best_split = unique_x[i];
        }
    }

    return Rcpp::List::create(
        Rcpp::Named("best_ig") = best_ig,
        Rcpp::Named("best_split") = best_split
    );
}

Rcpp::List getBestSplitNum(const NumericVector &y, const NumericVector &x) {
    NumericVector unique_x = unique(x);
    unique_x.sort();

    LogicalVector mask;
    int s1, s2, sz = y.size();
    double ig, best_split = 0, best_ig = -1, vary = Var(y);

    for (int i = 0; i < unique_x.length(); i++){
        mask = x < unique_x[i];
        s1 = sum(mask);
        s2 = sz - s1;

        if(s1 < MIN_SAMPLES_LEAF || s2 < MIN_SAMPLES_LEAF){
            continue;
        }

        ig = vary - (double(s1)*Var(y[mask]) + double(s2)*Var(y[!mask]))/double(sz);
        if(ig > best_ig){
            best_ig = ig;
            best_split = (unique_x[i] + unique_x[i - 1])/2;
        }
    }

    return Rcpp::List::create(
        Rcpp::Named("best_ig") = best_ig,
        Rcpp::Named("best_split") = best_split
    );
}

List getBestSplit(NumericVector &Y, SEXP X){
    if(TYPEOF(X) == 13 && Rf_isFactor(X)){
        return getBestSplitFct(Y, as<IntegerVector>(X));
    }else if(TYPEOF(X) == 16){
        return getBestSplitCat(Y, as<CharacterVector>(X));
    }else{
        return getBestSplitNum(Y, as<NumericVector>(X));
    }
}

Rcpp::List getFtSplit(NumericVector &y, DataFrame &xpred){ 
    List res, best_split(1);
    double best_ig = -1;
    string best_var;
    int idx = 0;

    // size = ncol
    for (int i = 0; i < xpred.size(); i++){
        res = getBestSplit(y, xpred[i]);
        if((double)res["best_ig"] > best_ig){
            best_ig = res["best_ig"];
            best_var = COL_NAMES[i];
            best_split = res["best_split"];
            idx = i;
        }
    } 

    if(best_ig > MIN_IG)
        ::FI[idx] += (double(y.length())/double(::N) * best_ig);


    return Rcpp::List::create(
      Rcpp::Named("best_ig") = best_ig,
      Rcpp::Named("best_var") = best_var,
      Rcpp::Named("best_split") = best_split[0]
    );
} 

Rcpp::List splitDF(DataFrame &X, string &best_var, List &split){
    LogicalVector mask(X.nrows());

    // string
    if(TYPEOF(X[best_var]) == 16){
        CharacterVector x = as<CharacterVector>(X[best_var]);
        mask = get_mask(x, split[0]);
    }else{
        NumericVector x = as<NumericVector>(X[best_var]);
        if(x.hasAttribute("levels")){
            mask = x == split[0];
        }else{
            mask = x <= split[0];
        }
    }

    IntegerVector idx = seq(1, X.nrows());
    DataFrame df1 = subsetDF(X, idx[mask]), df2 = subsetDF(X, idx[!mask]);

    // for (int i = 0; i < X.size(); i++){
    //     string name = string(::COL_NAMES[i]);        
    //     if(TYPEOF(X[i]) == 13){
    //         IntegerVector x = as<IntegerVector>(X[i]);
    //         df1[name] = x[mask];
    //         df2[name] = x[!mask];
    //     }else if(TYPEOF(X[i]) == 16){
    //         CharacterVector x = as<CharacterVector>(X[i]);
    //         df1[name] = x[mask];
    //         df2[name] = x[!mask];
    //     }else{
    //         NumericVector x = as<NumericVector>(X[i]);
    //         df1[name] = x[mask];
    //         df2[name] = x[!mask];
    //     }
    // }

    // kanan <= dan ==
    // kiri > dan !=
    return Rcpp::List::create(
        Rcpp::Named("df1") = df1,
        Rcpp::Named("df2") = df2,
        Rcpp::Named("mask") = mask
    );
}

Rcpp::List generateNode(NumericVector &Y, DataFrame &X, int DEPTH, IntegerVector &IDX){
    int n_t = Y.length();
    if((DEPTH + 1 <= MAX_DEPTH) && (n_t >= MIN_SAMPLES_SPLIT) && (n_t >= 2*MIN_SAMPLES_LEAF)){
        List res = getFtSplit(Y, X);

        std::string BEST_VAR = res["best_var"];
        double BEST_IG = res["best_ig"];
        List SPLIT = res["best_split"];
        
        if((BEST_IG >= MIN_IG)){
            List L = splitDF(X, BEST_VAR, SPLIT);

            LogicalVector mask = L["mask"];
            DataFrame df1 = Rcpp::as<DataFrame>(L["df1"]);
            DataFrame df2 = Rcpp::as<DataFrame>(L["df2"]);
            NumericVector y1 = Y[mask], y2 = Y[!mask];
            IntegerVector idx1 = IDX[mask], idx2 = IDX[!mask];

            return Rcpp::List::create(
                Rcpp::Named("pos") = "node",
                Rcpp::Named("best_var") = BEST_VAR,
                Rcpp::Named("split") = SPLIT[0],
                Rcpp::Named("ig") = BEST_IG,
                Rcpp::Named("left") = generateNode(y1, df1, DEPTH + 1, idx1),
                Rcpp::Named("right") = generateNode(y2, df2, DEPTH + 1, idx2)
            );
        }
    }

    List lm = fastLM(Y, X);
    NumericMatrix coef = lm["coefficients"];
    ::PREDICTION[IDX] = predictLM(coef, X);    

    return Rcpp::List::create(
        Rcpp::Named("pos") = "leaf",
        Rcpp::Named("coef") = coef,
        Rcpp::Named("mean") = rata2(Y)
    );
    
}

// [[Rcpp::export]]
Rcpp::List cartReg(NumericVector &y, DataFrame &x, int max_depth, int min_samples_leaf, int min_samples_split, double min_ig){
    ::FI = rep(0.0, x.size());
    ::COL_NAMES = x.names();
    ::N = y.length();
    ::PREDICTION = NumericVector(y.begin(), y.end());
    ::MAX_DEPTH = max_depth;
    ::MIN_SAMPLES_LEAF = min_samples_leaf;
    ::MIN_SAMPLES_SPLIT = min_samples_split;
    ::MIN_IG = min_ig;
    IntegerVector idx = seq(0, ::N - 1);

    // LogicalVector is_numeric(x.size());
    List is_numeric(x.size());
    for (int i = 0; i < x.size(); i++){
        if(TYPEOF(x[i]) == 13 && Rf_isFactor(x[i])){
            is_numeric[i] = false;
        }else if(TYPEOF(x[i]) == 16){
            is_numeric[i] = false;
        }else{
            is_numeric[i] = true;
        }
    }
    is_numeric.attr("names") = COL_NAMES;

    List res = generateNode(y, x, 0, idx);
    ::FI = ::FI/sum(::FI);

    return Rcpp::List::create(
        Rcpp::Named("feature_importance") = DataFrame::create(
            _["variable"] = COL_NAMES,
            _["value"] = FI
        ),
        Rcpp::Named("tree") = res,
        Rcpp::Named("is_numeric") = is_numeric,
        Rcpp::Named("prediction") = PREDICTION
    );
};



void _pred(List res, DataFrame x, IntegerVector idx){
    if(as<string>(res["pos"]) == "leaf"){
        if(!METHOD.compare("mean")){
            PREDICTION[idx] = double(res["mean"]);
        }else{
            NumericMatrix bmat = res["coef"];
            PREDICTION[idx] = predictLM(bmat, x);
        }
    }else{
        string col = res["best_var"];
        LogicalVector mask(x.nrows());

        if(TYPEOF(x[col]) == 14){
            mask = as<NumericVector>(x[col]) <= res["split"];
        }else{
            if(TYPEOF(x[col]) == 16){
                CharacterVector y = as<CharacterVector>(x[col]);
                mask = get_mask(y, Rcpp::as<std::string>(res["split"]));
            }else{
                IntegerVector y = as<IntegerVector>(x[col]);
                mask = y == res["split"];
            }
        }

 
        IntegerVector irow = seq(1, idx.length());

        int sumMask = sum(mask);
        if(sumMask == mask.length()){
            _pred(res["right"], subsetDF(x, irow), idx);
        }else if(sumMask == 0){
            _pred(res["left"], subsetDF(x, irow), idx);
        }else{
            _pred(res["right"], subsetDF(x, irow[!mask]), idx[!mask]);
            _pred(res["left"], subsetDF(x, irow[mask]), idx[mask]);
        }
    }
}

// [[Rcpp::export]]
Rcpp::NumericVector predictCart(Rcpp::List &model, DataFrame &X, std::string method = "mean"){
    int N = X.nrows();
    IntegerVector idx = seq(0, N - 1);
    ::PREDICTION = NumericVector(idx.begin(), idx.end());
    ::METHOD = method;
    // ::IS_NUMERIC = model["is_numeric"];

    // List res = model["tree"];
    _pred(model, X, idx);
    return PREDICTION;
}