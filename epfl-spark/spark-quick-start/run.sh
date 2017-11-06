#!/bin/bash
~/code/spark-2.2.0-bin-hadoop2.7/bin/spark-submit --class "SimpleApp" --master local[2] \
target/scala-2.11/simple-project_2.11-1.0.jar
