from collections import defaultdict
import numpy as np
import pandas as pd

# Constants describing the kind of constraint. For a minimum constraint, the
# sum of all the decision variables has to be greater than or equal to a
# constant, and so on. These are printed for lpsolve to consume.
MIN_CONSTRAINT = '>='
MAX_CONSTRAINT = '<='
EQ_CONSTRAINT = '='


class Constraint(object):
    def __init__(self, ids, comparator, constraint):
        """Constrain the number of items with a given property selected

        This exists in order to easily print constraints of the type
        Select <= 2 items about content area A. If the items from content area
        A are 1, 3, 5, and 10, we'd want to print the constraint as
        x1 + x3 + x5 + x10 <= 2;
        This also makes it easy to print the same constraint with an offset,
        which is desirable when printing for many forms at once.
        """
        self.ids = ids
        self.comparator = comparator
        self.constraint = constraint

    def as_lpsolve_string(self, form):
        """Returns this constraint as a string fit for lpsolve

        Prints this constraint for lpsolve
        Example: suppose
            form=2
            self.ids = [x5, x7]
            self.comparator is 1
            self.constraints is '<'
            Then we would return
            x5_2 + x7_2 < 1;
            which lpsolve can understand

        Arguments:
        form: The form id
        """
        var_names = map(lambda x: x + '_' + str(form), self.ids)
        return (' + '.join(var_names) +
                self.comparator + str(self.constraint) + ';\n')


class ConstrainedAttribute(object):

    def __init__(self, classifications, comparator, constraints, ids):
        """Initialize a set of constraints

        Arguments:
            classifications: an ordered NxM array with the classification for
                all N items we're concerned with, for each of M constraints.
            constraints: A dictionary mapping classifications onto ints
            representing the limit on items with that classification.
            comparator: one of '<=', '>=' or '='; determines whether we have
                minimum, maximum, or equality constraints.
        """
        self.constraints = []
        zeros = np.zeros(ids.size)
        classifications = [classifications]
        for categories, limit in constraints.iteritems():
            categories = [categories]
            # items = np.where(classifications == category)[0]
            items = np.ones(ids.size)
            for classification, category in zip(classifications, categories):
                items = np.where(classification == category, items, zeros)
            self.constraints.append(
                Constraint(ids[items == 1], comparator, limit))
            print len(ids[items == 1]), comparator, limit, category

    def as_lpsolve_string(self, form):
        """Returns these constraints as a string fit for lpsolve

        See Constraint.as_lpsolve_string for more information

        form: the form id
        """
        return ''.join(c.as_lpsolve_string(form) for c in self.constraints)


class MultiConstrainedAttribute(ConstrainedAttribute):

    def __init__(self, classifications, comparator, constraints, ids):
        """Initialize a set of constraints on multiple attributes

        Arguments:
            classifications: a list of Nx1 arrays with the classification for
                all N items we're concerned with, in order.
            constraints: A dictionary mapping classifications onto ints
            representing the limit on items with that classification.
            comparator: one of '<=', '>=' or '='; determines whether we have
                minimum, maximum, or equality constraints.
        """
        self.constraints = []
        for category, limit in constraints.iteritems():

            items = np.where(all(classifications[i] == category[i] for i in
                             range(len(classifications))))[0]

            self.constraints.append(Constraint(ids[items], comparator, limit))


class Solver(object):

    def print_constraint(self, decision_vars, comparator, total):
        return ' + '.join(decision_vars) + comparator + str(total) + ';\n'

    def __init__(self, data, constraints, num_forms, items_per_form):
        """
        Arguments:
        data: a dataframe of items and their parameters
        constraints: a json with the constraints for each content area
        num_forms: an int representing the total number of forms we're creating
        items_per_form: an int representing the number of items per form
        """
        self.num_forms = num_forms
        self.items_per_form = items_per_form

        # i is just the total number of items
        self.num_items = len(data)
        self.ids = data.item_id

        # m is the total number of decision variables. We're deciding whether
        # each
        # item is in each form, so we have i * f variables.
        self.decision_vars = self.num_items * self.num_forms

        # Create a dictionary listing the decision variable name for every
        # instance of an item. For instance, if we're looking at item
        # 3, and we have 4 forms and 10 items, this would yield a dictionary
        # for which 3's value would be ['x3', 'x13', 'x23', 'x33']
        self.item_occurences = {
            item: [self._var_for(item, form) for form in range(self.num_forms)]
            for item in range(self.num_items)}

        # Create a dictionary listing the decision variable name for every
        # item in each form. For instance, if we have 3 items and two forms,
        # this w
        self.form_items = {
            form: [self._var_for(item, form) for item in range(self.num_items)]
            for form in range(self.num_forms)}

        self.constraints = []
        for category, constraints in constraints.iteritems():
            for constraint in constraints:
                self.constraints.append(ConstrainedAttribute(
                    data[category], constraint['comparator'],
                    constraint['values'], self.ids))

        self.var_names = [
            self._var_for(item, form)
            for item in range(self.num_items)
            for form in range(self.num_forms)]

    def _var_for(self, item, form):
        return self.ids[item] + '_' + str(form)

    def solve(self, outfilename):
        """Convert all of the constraints to strings and write them to a file
        """
        with open(outfilename, 'w') as lp:

            # This is a technicality - we are setting this up an a minimization
            # problem, though for now there is nothing we are minimizing.
            lp.write('min: ;\n')

            # This adds a constraint that each item can only exist in one form.
            # It guarantees that an the sum of all occurrences of item i is <=
            # 1.
            for i in range(self.num_items):
                lp.write(
                    self.print_constraint(self.item_occurences[i], '<=', 1))

            # Ensure there are exactly n items in each form
            for f in range(self.num_forms):
                lp.write(self.print_constraint(
                    self.form_items[f], '=', self.items_per_form))

            # Add constraints on the amount of content in each category
            # We added the constraints in the initialization, but here we
            # print them for each form.
            for form in range(self.num_forms):
                for constraint in self.constraints:
                    lp.write(constraint.as_lpsolve_string(form))

            # All of the decision variables are binary, as they represent
            # whether each item is in each form
            lp.write('bin %s;\n' % ', '.join(self.var_names))


def main(item_parameters_file='item_params.csv', num_forms=2,
         items_per_form=18, constraints=None, outfile='run.lp'):
    """Write a optimization problem for lpsolve to solve

    This code creates a relatively easy way to create tests with arbitrary
    properties from a set of items.

    Input:
    item_parameters_file is the name of a csv file with stats on each of the
        items of interest.
    num_forms: The total number of tests we're assembling
    items_per_form: the number of items in each tests
    constraints: A nested dictionary of the form:
        {content_column : [{"comparator": some comparator(e.g. '<='),
                            "values": {constrained_tag: value}}]}
        This would likely be encoded in some sort of configuration file.
    """

    # There could potentially be many constraints, along many axes. For
    # instance, we could have content requirements as described below - between
    # 0 and 10 items of category 5 - but we could also have constraints on
    # difficulty, which would be a different column in the input data, and a
    # third constraint on the gender of the characters in any problem with
    # context.
    # Here we construct a default constraint set that works with the default
    # data.
    if not constraints:
        constraints = defaultdict(list)
        constraints['Content'].append(
            {'comparator': MIN_CONSTRAINT,
             'values': {5: 0, 1: 0, 2: 0, 3: 0, 4: 0}})
        constraints['Content'].append(
            {'comparator': MAX_CONSTRAINT,
             'values': {5: 10, 1: 10, 2: 10, 3: 15, 4: 10}})

    data = pd.read_csv(item_parameters_file)
    s = Solver(data, constraints, num_forms, items_per_form)
    s.solve(outfile)


if __name__ == '__main__':
    main()
