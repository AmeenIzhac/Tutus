# Tutus Artefact README

Our paper introduces Tutus, a code generation tool for applying crash handling  
patterns to Scribble protocols, and generating corresponding Scala code using
the Effpi concurrency library.

## Quick Start

### macOS 🍎
Install Homebrew if it's not already installed, then run:

```brew install haskell-stack```

### Windows

Download the Stack installer from the official site:  https://get.haskellstack.org/stable/windows-x86_64-installer.exe

### Build the project

```stack build```

## Running the Tool

The following command will apply the Graceful Failure pattern to the first example protocol in the scribble/ folder:

```stack exec Tutus -- --file=scribble/a_PingPongAll.nuscr --refactorgf```

The output consists of the refactored protocol followed by an integer representing the refactoring time in microseconds.
The other uses of the tool can be leveraged via the following options:

- to generate Scribble code via the Graceful Failure Pattern: ```--refactorgf``` 
- to generate Scribble code via Local Graceful Failure Pattern: ```--refactorlgf``` 
- to generate Scribble code via Local Failover Pattern: ```--refactorfo``` 
<!-- - to generate Scala code for a well formed Scribble protocol: ```--effpi``` 
- to generate Scala code for Failover Scribble protocol: ```--effpifo``` 
- to generate Scala code for Failover Scribble protocol:  -->
