Encoding multiple rows of data values to a single data symbol produces
a **visual shape**, like a line on a line plot.

The main benefit of a data symbol that is a visual shape
is that data summaries, such as modes, skewness, local maxima and minima,
and trends over time, can can be decoded 
from a visual shape.
On the downside, 
decoding raw data values from a visual shape may be harder, compared
to decoding raw data values from a simple data symbol like a bar.

One danger with visual shapes is that they depend on aspect ratio 
and scale.
The same encodings can be made to decode to different data summaries,
so we are responsible for selecting an appropriate aspect ratio and scale.

Another danger is that a visual shape may not necessarily convey 
a useful data summary.
We need to only create visual shapes in a purposeful manner
and avoid accidentally creating visual shapes that may confuse or mislead.
