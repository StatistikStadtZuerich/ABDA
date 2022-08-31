# ABDA
Shiny app for ABDA Tool (Abstimmungsdatenbank)

# Architektur

```mermaid
flowchart LR;
  f1[F1 text search] --> button[button start]
  f2[F2 select date range] --> button
  f3[F3 select pol level] --> button
  button --> output1[filtered data = F1 + F2 + F3]
  output1 --> results1[Resultat Vorlagen]
  results1 --> f4[F4 selectVote]
  f4 --> output2[output1 + F4]
  output2 --> results2["Resultat Vorlagen (voteResult)"]
  output2 --> downloads[Downloads]
  classDef filter fill:#e0db41,stroke:#e0db41;
  classDef data fill:#edade6,stroke:#edade6;
  classDef result fill:#59e6f0,stroke:#59e6f0;
  classDef downloads fill:#43cc4c,stroke:#43cc4c;
```
