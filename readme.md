## Ada configuration

Gnat should be working if you installed gcc

Gprbuild source:

> https://github.com/alire-project/GNAT-FSF-builds/releases

Download, place somewhere, add bin folder to path and you should be good

Info src:

> https://www.noureddine.org/articles/ada-on-windows-and-linux-an-installation-guide

## Tasks

-   [x] Consumer should handle not receiving a assembly
-   [x] Producer should handle not being able to put something into Buffer
-   [x] Balance Buffer
-   [ ] Implement Furious_Worker as specified in the instruction
-   [ ] Fix janky implementation of removing a product in Can_Accept function, should remove in Take entry after Can_Accept returns true; Maybe add out ShouldRemove : Boolean as a argument in Can_Accept?
