**Colour** is really three visual features:  **hue**, **chroma**, 
and **luminance**.

**Hue** is excellent for encoding **nominal** data values, though
it has a limited capacity.

**Chroma** and **luminance** can be used to encode **ordinal**
data values (as well as nominal data values), but they have even lower capacity.

When we encode data values as colours
there are several caveats:

* The decoding of data values from colours
  is affected by surrounding colours and the size of the data symbol.

* Approximately 10% of viewers are unable to differentiate between
  red and green hues with similar chroma and luminance.

Selecting which colours should be used to encode data values is
difficult to get right and a good solution often involves
varying all of hue, chroma, and luminance at once.

Consequently, it is usually a good idea to make use of pre-existing
colour palettes that have been carefully designed to avoid most
problems.  
