
# cedar_abm

<!-- badges: start -->
<!-- badges: end -->

The goal of cedar_abm is to create a basic ActivitySim specification for a small 
town. This includes the following:

  - A synthetic population
  - A set of skims created by `r5r` representing transit, bike, drive, and walk skims
  - Basic choice model coefficients estimated from the national household travel survey
  
  
## Environment

The skims are built using the [`r5r`](https://github.com/ipeaGIT/r5r) library,
which in turn uses the `r5` routing library through an `rJava` interface. As a
result, you must have the proper version of Java indexed. The easiest way 
to do this is by starting your session with an `.Renviron` file containing the following
line (replacing with your path to Java 11:

```
JAVA_HOME="/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home"
```



