When we visualise multiple variables at once, it is not
effective to encode each different variable to a different visual feature
because that usually forces us to make use of a visual feature that is
either inappropriate or inaccurate.

An alternative is to reuse the same visual feature for multiple
different variables.
In particular, we can reuse **position** for more than 
just two variables, for example, to create a scatter plot matrix
or a facetted plot.
This at least allows us to accurately decode individual variables.

Another alternative is to use non-cartesian coordinates, for
example 3D plots or parallel coordinates plots.
These generate visual shapes that allow us to decode multivariate
data summaries, like multivariate clusters and multivariate correlations.
However, in order to gain multivariate data summaries,
we typically have to sacrifice the ability to accurately decode individual
data values.

No matter which approach we use,
there is still a limit to how much information can effectively be displayed at
once within a static data visualisation.
This is one area where dynamic and interactive graphics can be useful
because the viewer is able to rapidly switch between multiple views
of the data.  
