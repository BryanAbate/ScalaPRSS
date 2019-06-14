# ScalaPRSS

## What is ScalaPRSS
ScalaPRSS aims to implement the theory of secret sharing brought by Cramer et al. (pseudorandom secret sharing) and use it to do threshold Schnorr signature and threshold ElGamal encryption. Two secret sharing methods are implemented: the original one brought by Adi Shamir and the pseudorandom secret sharing mentioned above which is the core of this project. The advantage of the pseudorandom secret sharing implementation over the Adi Shamir one is that while setup time is a bit long, subsequent secret sharing are really fast.

The implementation is done in Scala and use Kalium, the Java binding of the cryptography library Libsodium.

You can use this project either as a library inside your project to do secret sharing either as an exectuable jar for encryption/decryption.

This project was realised as my Bachelor project during my Erasums exchange at the University of Bristol.

## Prerequisite

**Be careful not to have any spaces in your path to this folder.**

To just run the executable you will need Java 8 or higher.

To compile or run the tests you will need:

* Java 8 or higher
* Scala 2.11.5
* Maven 3
* Libsodium

## Compile
To compile the sources (tests will run before compiling) do:

`mvn package`

The executable jar file can then be found in the *target* folder with the name:

`pseudorandom_secret_sharing-1.0-jar-with-dependencies.jar`

## Run the program
You can either compile the sources or get the executable jar in the *executable* folder. 

**Important note:** The executable jar provided works on Linux (at least on Ubuntu) but does not work on Windows or Mac OS, for the executable jar to work on these OS, it has to be compiled on these OS, this comes from the way Libsodium is bound.

To run the program do:

`java -jar pseudorandom_secret_sharing-1.0-jar-with-dependencies.jar`

To add arguments to the program, for exemple to get the help page, do:

`java -jar pseudorandom_secret_sharing-1.0-jar-with-dependencies.jar -help`

Or to encrypt a file *file_name* using PRSS and ElGamal encryption with 5 participants and threshold value 3, do:

`java -jar pseudorandom_secret_sharing-1.0-jar-with-dependencies.jar -PRSS encrypt 3 5 file_name`
## Run the tests only
Do `mvn test`

## See the scaladoc
Open index.html inside the scaladocs folder.

## Needed features
* Move from Maven to sbt
* Make the library available as an sbt package