# Collision Avoidance on Drones

The aim of this project is the analysis and the implementation of a
distributed solution for collision avoidance between drones in a 2 dimensional space.

The problem can be divided in 3 main subproblems: drone synchronization, collision detection and priority agreement.

Note that drones are autonomous systems without any external air
traffic control and can send messages to other drones using wireless technology in order to communicate. 

The system must be also robust to failures especially those regarding drones and must always guarantee that a
delivery must eventually be completed. The project implements a simulator for the environment, business logic and a Rest API that exposes
the simulation state for any future UI Implementations.

## Team
* Luca Rosso
* Andrea Giuseppe Zarola

## Dependecies
In order to run the project you will need:
* docker >= v23.0.1
* docker-compose >= v2.16.0.

## Getting started
For starting the distributed system locate to ./src directory

docker-compose up

For starting the UI Control Service locate to ./src/ui_control_service

./docker-run

## Environment variables
All these variables can be found in the drone-hub-variables.env
* DEV_MODE: when is enabled the drones are spawned in the same node of the drone_hub otherwise every drone will have its own container.
* VELOCITY: speed of flight for the drones (m/s).
* DRONE_SIZE: Size of the bounding box.
* FLY_HEIGHT: height to which drones fly.
* NOTIFY_THRESHOLD: Threshold that allow a drone to get priority against the other drones.
* RETRY_LIMIT: Max number of retries that a drone do in order to synchronize with other drones(also defined in the Dockerfile of drone).
* HOST_PATH_VOLUME: directory to which logs files are stored.

All these variables can be found in ./src/rest-variables.env
* MAX_SIZE: size of the cartesian plane
