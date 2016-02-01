# CampMyTrip  
A program to find the optimal campgrounds along a route.

Currently there are two scripts:  
  + *download_campgrounds.R*:   Used to download all data on campgrounds from the US and convert to a csv file.
  + *CampMyTrip.R*:   Currently conducts exploratory analysis of data and generates two figures (see below).  


**Figure 1. Number of campgrounds in the ReserveAmerica database within 25km of U.S. Interstate Highways.**  
Campground locations are indicated by grey triangles while interstates are colored by the number of campgrounds. Distances from highways to campgrounds were calculated directly and not via roads.  
![Fig 1](https://github.com/jescoyle/CampMyTrip/blob/master/campsites_25km_highres.png)

**Figure 2. Campgrounds mid-route from my home (San Francisco, CA) to my in-laws (Corvallis, OR).**  
Figure shows the distance of the campground from my home and the extra time it would take to drive there. Campgrounds are colored by the operating agency.  
![Fig 2](https://github.com/jescoyle/CampMyTrip/blob/master/campgrounds_homeToinlaws.png)

