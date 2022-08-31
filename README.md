# ABDA
Shiny app for ABDA Tool (Abstimmungsdatenbank)

# Architektur

```mermaid
flowchart LR;
  f1[F1 text search]:::filter --> button[button start]:::button
  f2[F2 select date range]:::filter --> button
  f3[F3 select pol level]:::filter --> button
  button --> output1[(filtered data = \nF1 + F2 + F3)]:::data
  output1 --> results1[[Resultat Vorlagen]]:::result
  results1 --> f4[F4 selectVote]:::filter
  f4 --> output2[(output1 + F4)]:::data
  output2 --> results2[["Resultat Vorlagen \n(voteResult)"]]:::result
  output2 --> downloads{Downloads}:::download
  
  classDef filter fill:#ffff2f,stroke:#ffff2f;
  classDef button fill:#695eff,stroke:#695eff;
  classDef data fill:#edade6,stroke:#acb0b0;
  classDef result fill:#59e6f0,stroke:#acb0b0;
  classDef download fill:#43cc4c,stroke:#43cc4c;
```
