# Eagle : Bringing Symbolic Verification to OWL

Symbolic and computational cryptographic verification frameworks are essential for ensuring the security and
correctness of modern cryptographic protocols. Eagle is a lose subset of the OWL language,
designed to bridge the gap between symbolic and computational verification. Eagle leverages the Sapic+
framework, an applied pi-calculus, to translate cryptographic protocols into symbolic representations suitable
for verification in Tamarin, a state-of-the-art symbolic verifier.

### How to run
```main.ml``` contains two string variables, ```fin``` and ```fon```, the first reads an eagle file input string and outputs it to the location of the output string.

1. Change ```fin``` and ```fon``` to your desired input file and output location
2. Run translation by ```dune exec eagle --profile release```
3. The tamarin file will then be generated at the ```fon``` location  
4. To run the tamarin file do ```tamarin-prover interactive <file name>```


### How to install

To install Tamarin and Sapic+ follow directions in the tamarin manuel https://tamarin-prover.com/manual/master/book/002_installation.html 

To install Dune follow the directions at https://dune.build/install


