# This is adapted from the code in http://goo.gl/zw2eeq
# It has been edited for clarity and style, and comments have been added.
# This version is for math only

# Arguments:
# item_parameters_file: a csv of items and their parameters that
#   we'll optimize over
# content_minimums: A list with the minimum values for each content category
#   The content categories should be named 1 - n, with each item in
#   content_minimums corresponding to the corresponding category.
# num_forms: an int representing the total number of forms we're creating
# items_per_form: an int representing the number of items per form
solver <- function(item_parameters_file, content_minumums, num_forms, n){

    # Constants
    # These are the ability  values for which we care about maximizing the TIF
    theta = c(-1.5,0,1.5);
    # These are target information levels for the three ability levels we're
    # optimizing for in our optimization function
    d_theta = c(5.4, 10, 5.4);

    # Manually set constants end here!

    # Import the lpsolve library
    library(lpSolveAPI);

    # Read in the data. This should be of the form item,a,b,c,category
    # This example involves IRT for the Test Information Function
    s = read.csv(item_parameters_file);

    # The 3PL model for items has these three variables, a, b, and c. Learn more
    # on wikipedia: http://en.wikipedia.org/wiki/Item_response_theory

    # a is discrimination
    a = s$a;

    # b is difficulty
    b = s$b;

    # c is a psudo-guessing offset
    c = s$b;

    # Content is the different categories - like 1, 2, 3, 4, 5, 6...
    content = s$Content;

    # i is just the total number of items
    num_items = nrow(s);

    # This is the number of content categories. We assume they're labeled
    # 1-num_categories for the sake of simplicity
    num_categories = length(unique(content));

    # This is where we sort all of our items by content.
    content_items=list();
    for(k in 1:num_categories){
        content_items[[k]] = c(1:num_items)[content==k];
    }

    # This i x j array contains the amount of information item i gives us
    # about theta value j
    j=length(theta);
    info = array(0,c(num_items, j));

    # This calculates the information at each value
    # http://en.wikipedia.org/wiki/Item_response_theory#Three_parameter_logistic_model
    # euler's constant, e, can be represented as exp(1)
    e = exp(1)
    for (jj in 1:j){
        # P is the probability of answering correctly
        p = c + (1-c) / (1 + exp(-e * a * (theta[jj]-b)));
        q = 1 - p;

        # src:de Ayala, R.J. (2009). The Theory and Practice of Item Response
        # Theory, New York, NY: The Guilford Press. (6.12), p.144
        info[,jj] = (e^2) * (a^2) * ((p - c)/(1 - c))^2 * q / p;
    }

    # m is the total number of decision variables. We're deciding whether each item
    # is in each form, so we have i * f variables. (TODO: What is the last one)
    m = num_items * num_forms + 1;

    # CONFIGURE THE LINEAR PROGRAM

    # Here's our linear program object
    # We start with 0 rows and m columns. Rows are added as constraints are added.
    lprec = make.lp(0, m);

    # The sense control means we're minimizing the objective function
    # epsint is the tolerance to decide if a floating point number is an integer
    # The mip gap has to do with the algorithm used and what solution branches to
    # ignore to speed things up.
    # See http://lpsolve.sourceforge.net/5.5/set_mip_gap.htm
    lp.control(lprec, sense="min", epsint=0.1, mip.gap = c(0.1,0.05));

    # All of the first i * f columns are binary, as they represent whether each
    # item is in each form
    set.type(lprec, columns=c(1:(num_forms * num_items)), type="binary");

    # The last decision variable is real
    set.type(lprec, columns=m, type="real");

    # Each variable must range between 0 and 1
    set.bounds(lprec, lower=rep(0, m), upper=rep(1, m));

    # ADD CONSTRAINTS TO THE LINEAR PROGRAM

    # This adds a constraint that each item can only exist in one form.
    # This actually will only work for two forms max.
    # It's basically guaranteeing that an array of ones times an array of
    # whether item i appears in form f for each i and all f is <= 1.
    for (i in 1:num_items){

        idxs = c();
        for (f in 1:num_forms){
            item_offset = num_items * (f - 1);
            idxs = c(idxs, i + item_offset);
        }
        add.constraint(lprec, rep(1, num_forms), "<=", 1, indices=idxs);
    }

    # Add constraints on the amount of content in each category
    for (f in 1:num_forms)
    {
        item_offset = num_items * (f - 1);
        for (k in 1:length(content_minumums)) {
            add.constraint(
                lprec,
                rep(1, length(content_items[[k]])), ">=", content_minumums[k],
                indices=item_offset + content_items[[k]]);
        }
    }

    # Ensure there are exactly n items in each form
    for (f in 1:num_forms)
    {
        first_item = num_items * (f - 1) + 1;
        last_item = num_items * f;
        add.constraint(lprec,
                       rep(1, num_items), "=", n,
                       indices=first_item:last_item);
    }

    # Add constraints on the information level
    for (f in 1:num_forms)
    {
        first_item = num_items * (f - 1) + 1;
        last_item = num_items * f;

        # Add a constraint for each theta level
        for (k in 1:length(d_theta)) {
            add.constraint(lprec,
                           c(info[,k], -1), "<=", d_theta[k],
                           indices=c(first_item:last_item,m));
            add.constraint(lprec,
                           c(info[,k], 1), ">=", d_theta[k],
                           indices=c(first_item:last_item,m));

        }
    }

    # Set the objective function
    set.objfn(lprec, 1, indices=m);

    res_flag = solve(lprec);

    res_flag;

    # Now x_opt contains the assignments of questions to forms
    # TODO: Print out an understandable list of Form #, Item #
    x_opt = get.variables(lprec);

    return(x_opt);
}
