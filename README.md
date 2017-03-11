# VoiceExperiment

## An R-Package for Offline Analysis of Spoken Responses

This Package can be used to analyse spoken respones in psychological experiments. It can be used to extract voice onset times and answer
categories from sound files stored during an experiment (for instance run in Psychopy or Opensesame). The packge both provides functions
for analysis, which can be used as building blocks to build your own analysis chain, as well pre-defined analysis functions, which can be
used to analyse a complete directory of sound files at once and provide the results either as a data.frame of as a csv file which is
written to disk.

## Usage

### Installation

The software can be installed directly from github using the devtools package. Just type

```R
devtools::install_github("LiKao/VoiceExperiment")
```

at the R prompt to install the software. If you don't have devtools installed currently, please install the devtools package via the
command

```R
install.packages("devtools")
```

### Running the Analysis

After loading the package via

```R
library("VoiceExperiment")
```

a complete analysis of the onsets can be triggered using the command

```R
expOnsets.as.csv("TestData","test.csv")
```

This command will take the files in the directory "TestData" and write the results to a file called "test.csv" in the current working
directory (use "getwd()" to find the current working directory and "setwd()" or 
"Session" -> "Set Working Directory" -> "Choose Directory" in RStudio to change the working directory).

If you want to get the analysis results as a data structure within R, you can use the command 

```R
res <- analyse.directory.onsets("TestData")
```

instead. This function will create a data structure that can be used directly or converted to a data.frame using "as.data.frame".

If you want to also classify responses you can use the function

```R
expResponses.as.csv("TestData","test.csv",nresponses=...)
```

where the paramter nresponses indicates the number of possible responses that were used during the experiment. Similarly

```R
res <- analyse.directory("TestData",nresponses=...)
```

will create an R data structure that both contains onsets as well as types of responses.

All parameters of the analysis can be changed for all commands. Please have a look at the package documentation to see how each parameter
influences the analysis.


## Internals

### Onset Detection

Onset detection is performed by converting the audio data into energy (i.e. the loudness of the signal). The resulting energy curve is normalized
and scanned via a hysteresis method. I.e. an onset is detected if the energy crosses a certain threshold (default: 10% of maximum) and all parts
of the signal are considered part of the same response, until the energy falls below another threshold (default: 1% of maximum). The hysteresis
approach reduces the possibility of detecting rises in the energy curve that follow a quiet part of the response.

For each onset also the offset (time at which the energy in the signal crosses the boundary) is also recorded. The timestamps of onsets and offsets
can be used to split the signal into windows at which a response took place for later classification.

By default very short blocks (between onset and offset) are discarded as false positives.

### Fingerprinting

The resulting blocks are converted to a sequence of Mel Frequency Cepstral Coefficients (MFCCs) for analysis of the type of response. The 
MFCC sequencens are then averaged to provide a single fingerprint vector for the given block. This analysis is performed for the block with
the highest energy of each audio file in the complete analysis functions.

### Classificiation

Classification is based on a K-Means clustering of the responses. The number of clusters is determined based on the number of possible responses.
Each block is assigned the number of the cluster it belongs to.
