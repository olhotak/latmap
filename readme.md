Here is a summary of the changes that we (Jacob Jackson and Jim Zhang) made as part of our URA, which took place over the winter 2017 school term:

HashMapIndex - this is an implementation of the 'index' interface. It should behave in the same way as NaiveIndex, so if you want to understand what it's supposed to do, I recommend that you read NaiveIndex first. Although it is faster than NaiveIndex, it does not support concurrent updates. (It does support concurrent reads.)

ConcurrentHashMapIndex - this is modelled after HashMapIndex, but it supports concurrent updates. The structuring of these updates into phases is why the LatMap interface is somewhat complicated. This class is not yet fully concurrent; the writes themselves are OK, but the resize needs work.

SimpleLatMap - this class was changed to the point that it no longer lives up to its name. You attach indices to it and then follow the LatMap interface's specification for how you call its methods and then it will handle updating the indices properly.

IndexTest - does some basic performance comparisons of the various indices. This is good as a rough indicator, but at some point it would be good to add performance tests that are more reflective of actual usage patterns.
