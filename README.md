# Course projects in Information Retrieval
2015 Autumn, ETH Zürich

[Course website](http://www.da.inf.ethz.ch/teaching/2015/Information-Retrieval)

## Project 1: [web crawling](http://www.da.inf.ethz.ch/teaching/2015/Information-Retrieval/assignment1.php)
Running: run ```sbt run``` in the root folder of the project.

Building JAR: run ```sbt assembly``` in the root folder of the project. The JAR will be saved to ```target/scala-2.10/ir-2015-crawler-21.jar``` (or similar). The JAR can then be run with ```java -jar target/scala-2.10/ir-2015-crawler-21.jar``` (from the project root).

## Project 2: [queries](http://www.da.inf.ethz.ch/teaching/2015/Information-Retrieval/assignment2.php)
Running: run ```sbt run``` in the root folder of the project.

Data: the project root should contain a folder /data so that p2/data looks like:
* p2/data
  * allZips/
    * ap880212.zip
    * ...
    * zf2_354.zip
  * qrels
  * topics
