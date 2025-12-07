Encoding data values as the position of data symbols allows
for the effective decoding of both quantitative and qualitative information,
with the following caveats:

* For quantitative values, 
  what we can accurately decode are *comparisons* between quantitative 
  values, not absolute quantitative values.

* The decoding is most accurate for positions that share a common baseline.

* Encoding identical data values as the positions of data symbols
  means that the data symbols overlap, which compromises our 
  ability to decode data values from the data symbols.

* We can encode one set of data values as horizontal positions and
  another set of data values as vertical positions because
  we can decode horizontal
  and vertical positions separately.

* Decoding quantitative data values from the positions of data symbols
  is only accurate if the encoding is linear.
