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
To apply other patterns you may use the following options:

```--refactorlgf```

### Applying Crash Handing Patterns to Scribble

You can execute Tutus 


A quick (and partial) mockup of Fangyi's projection function to help speed up
experimenting with examples. Doing this by hand takes too long, and I don't
trust myself not to make a mistake. Why not experiment using nuScr? I can't
guarantee that nuScr's projection is the same as Fangyi's latest. We don't care
about the runtime types/forms so we omit those.

## Results metrics

- Projectable via nuScr (tick/cross)
  - nuScr's projection may differ from the paper's
  - this is not too great a problem
- EFSA generation time (i.e. nuScr) (it *is* part of generation...)
- Generation time (from EFSA)
- Size of protocol (by number of communication statements)
  - Rough idea of how large the protocol is, and how much it grows
- Maximum depth of crash-handling branch
  - Tells us how long/complicated they are

### Scaling examples

- Graphs showing generation time

## Notes/Questions/Comments for 20 October

25 pages for the camera ready version

- Examples
  - Literature Examples -- intentions & overview
    - POI: Travel Agency cannot be fully unreliable -- the resulting
      protocol would end up never terminating even in the presence of crashes.
      I wonder if this would be better if we could unroll recursions in
      projection -- i.e. merge \0 and End by unfolding \0 to ensure there
      aren't any further communications. Sounds a little far fetched.
  - Fault-Tolerance Examples -- intentions
- Failure handling behaviours -- what we have, what we hope for
  - We can't exhibit pure fail-over behaviours
- Projection problems
  - Suggestion for future projection definitions to have a greater awareness
    of crash handling semantics; we have at least one example where merging
    necessitates messages to a crashed process that the sender already knows
    has failed.
  - Question: why must merging Send actions have the same set of labels?
- JL's generator has some form of channel optimisation
  - Need to refresh myself on the details
  - We can use it as another results metric
  - Relatedly, do we want to describe the mapping from session to Scala types?
  - If so, I assume that we're okay using mine? JL's is a little inefficient
- Plan going forward
  - Will finish off the by-hand examples; ensuring projectability
    - Will generate the latex for them and add them to the paper
    - One paragraph explaining each, I think
  - Will pass them through JL's tool
    - Some question as to whether JL's tool will work out of the box
    - I really hope it does
  - Will then get actual numbers
  - Will do the scaling examples
  - Will then start writing in earnest

