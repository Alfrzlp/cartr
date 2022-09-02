Rcpp::List getUnique(const Rcpp::IntegerVector &v){
    // Initialize a map
    std::map<double, int> Elt;
    Elt.clear();

    // Count each element
    for (int i = 0; i != v.size(); ++i) {
        Elt[v[i]] += 1;
    }

    // Find out how many unique elements exist... 
    int n = Elt.size();

    // Pop the last n elements as they are already sorted. 

    // Make an iterator to access map info
    std::map<double, int>::iterator itb = Elt.end();

    // Advance the end of the iterator up to 5.
    std::advance(itb, -n);

    // Recast for R
    Rcpp::NumericVector result_vals(n);
    Rcpp::IntegerVector result_keys(n);

    unsigned int count = 0;

    // Start at the nth element and move to the last element in the map.
    for( std::map<double,int>::iterator it = itb; it != Elt.end(); ++it )
    {
        // Move them into split vectors
        result_keys(count) = it->first;
        result_vals(count) = it->second;

        count++;
    }

    return Rcpp::List::create(
            Rcpp::Named("lengths") = result_vals,
            Rcpp::Named("values") = result_keys
        );
    }

double rata2(NumericVector v){
    return std::accumulate(begin(v), end(v), 0.0) / v.size();
}

double Var(NumericVector v){
    const size_t sz = v.size();
    if(sz <= 1) return R_NaN;
    const double rata = rata2(v);

    auto variance_func = [&rata, &sz](double accumulator, const double val) {
        return accumulator + ((val - rata)*(val - rata) / (sz - 1));
    };

    return accumulate(v.begin(), v.end(), 0.0, variance_func);
}

LogicalVector get_mask(const CharacterVector &x, const string &split){
    LogicalVector mask(x.length());
    for (int j = 0; j < x.length(); j++){
        mask[j] = !split.compare(x[j]);
    }
    return mask;
}

Function subset("[.data.frame");
DataFrame subsetDF(DataFrame x, IntegerVector y) {
  return subset(x, y, R_MissingArg);
}