SET(ProjectName Storage_short)
PROJECT(${ProjectName})

CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

FIND_PACKAGE(OpenMP) 
if (OPENMP_FOUND) 
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif (OPENMP_FOUND)

# If you don't want to use openCV  and only need KVLD, 
# please commande the next line, 
# don't forget to also commande ${OpenCV_LIBS} further.
# FIND_PACKAGE( OpenCV REQUIRED )


#find Orsa and KVLD librarys
ADD_DEFINITIONS(-DSOURCE_DIR="${CMAKE_CURRENT_SOURCE_DIR}")

ADD_EXECUTABLE(${ProjectName} main.cpp basic.h basic.cpp )
# TARGET_LINK_LIBRARIES(${ProjectName} ${OpenCV_LIBS})