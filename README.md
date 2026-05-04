# The Immobilism Cascade — Replication Package

This repository accompanies the master's research project *"Dados que constroem ou destroem sua estratégia de inteligência artificial"* (FGV-EAESP, MPGC, 2026) and the manuscript *"The Immobilism Cascade How AI Initiatives Stall Between Pilot and Production"* under review at *MIS Quarterly Executive*.

The repository contains anonymized data, analytical scripts and supporting documents that allow other researchers to reproduce the qualitative-quantitative triangulation reported in the manuscript and to inspect the methodological audit trail.

---

## Authors and Contact

- **Romulo Banhe** (corresponding author) Master in Management for Competitiveness, FGV-EAESP. Email romulo.banhe@gmail.com
- **Eduardo de Rezende Francisco** Professor of Information Technology, FGV-EAESP. Email eduardo.francisco@fgv.br

---

## Manuscript and Dissertation

If you use materials from this repository, please cite

> Banhe, R. and Francisco, E. de R. "The Immobilism Cascade How AI Initiatives Stall Between Pilot and Production." Manuscript under review at *MIS Quarterly Executive*, 2026.

The full master's dissertation (in Portuguese) is included as `Romulo_Banhe_TA_Final_v6.pdf` and provides the complete theoretical framing, methodological detail and full discussion that the MISQE manuscript condenses.

---

## Ethical Statement

This research was conducted under FGV ethics protocol **P.310.2025** (approved 25 July 2025; see `Parecer.pdf`). Each of the 14 senior IT leaders interviewed signed an informed consent form (`Termo de Consentimento_v1.pdf`) that guaranteed confidentiality of their identities and restricted access to raw transcripts.

In compliance with the consent form

- **Raw interview transcripts are intentionally not included** in this public repository. They are stored in restricted access in accordance with the ethical commitment made to participants.
- All quantitative outputs in this repository (codebook, code-document frequency matrix, co-occurrence network, saturation curve, proposition tables) reference participants only by anonymized identifiers (E1 through E14).
- The single textual file (`bloco1_anonymized.txt`) used as input for term-frequency analysis has been systematically anonymized (62 substitutions covering institutions, employers, specific years and other identifiable markers).
- The Qualtrics consent log (`respostas_qualtrics.xlsx`) has been redacted to remove IP addresses, geolocation, response identifiers, recipient names, emails and timestamps; only the consent decision and recording authorization remain.

Researchers interested in the underlying interview data may contact the corresponding author. Access decisions will respect the original consent terms and the FGV ethics protocol.

---

## File Inventory

### Documentation and ethics

| File | Description | Manuscript reference |
|---|---|---|
| `README.md` | This file | — |
| `Termo de Consentimento_v1.pdf` | Informed consent form template signed by participants | §2 + Footnote 1 |
| `Parecer.pdf` | FGV ethics committee approval (CEPH P.310.2025) | §2 + Footnote 1 |
| `Instrumento_v1.pdf` | Research instrument interview script and invitation letter | §2 + Appendix A |
| `Romulo_Banhe_TA_Final_v6.pdf` | Full master's dissertation in Portuguese | Cited throughout |

### Codebook and analytical outputs

| File | Description | Manuscript reference |
|---|---|---|
| `TA – Code Listv2.xlsx` | 32-code codebook used in ATLAS.ti, with code groups and definitions | §2, Appendix A "32-code book" |
| `code-document_v1.xlsx` | Frequency matrix of 32 codes across 14 interviewees (E1-E14) | Appendix A "dominance analysis" |
| `cooccurence_nova.xlsx` | Co-occurrence matrix (33×33) used for network analysis | Figure 2 + Appendix A network weights cited in §3, §5 |
| `tabelas_A_B_proposicoes.xlsx` | Quotation-by-proposition tables (P1-P9) supporting triangulation | Discussion of propositions in dissertation; aggregated in manuscript |
| `resultados_saturacao.xlsx` | Saturation curve data (codes per interview) | §2 "saturation reached at the fourth interview" |
| `respostas_qualtrics.xlsx` | Anonymized Qualtrics consent responses (no PII) | §2 + Footnote 1 |

### Anonymized text input

| File | Description | Manuscript reference |
|---|---|---|
| `bloco1_anonymized.txt` | Concatenated Part-1 interview text, anonymized for TF-IDF analysis | Appendix A "sector-level term frequency analysis" |

### Analytical scripts (R)

| File | Description | Manuscript reference |
|---|---|---|
| `SCRIPT_TFIDF_BL1.R` | Term-frequency-inverse-document-frequency analysis script | Appendix A TF-IDF |
| `novo_script_mestre.R` | Master analysis script co-occurrence network, correspondence analysis, dominance, proposition synthesis | Appendix A "computational analyses summary" |

---

## Reproducing the Analysis

### Software requirements

- R version 4.0 or later
- R packages tidyverse, readxl, entropy, tidytext, factoextra, syuzhet, corrplot, igraph, ggraph, tm, scales, writexl, FactoMineR, dplyr, slam

### Setup

1. Clone this repository

```bash
git clone https://github.com/romulo-banhe/mpgc.git
cd mpgc
```

2. Open R or RStudio with the cloned directory as working directory.

3. Install required packages (the master script includes `install.packages()` calls)

```r
source("novo_script_mestre.R")
```

### Execution order

1. **`SCRIPT_TFIDF_BL1.R`** runs the TF-IDF analysis on `bloco1_anonymized.txt`. Produces sector-level lexical signatures.
2. **`novo_script_mestre.R`** runs the master analysis pipeline using `code-document_v1.xlsx`, `cooccurence_nova.xlsx` and `tabelas_A_B_proposicoes.xlsx`. Produces dominance analysis, network metrics, correspondence analysis and proposition synthesis.

### Notes on script paths

The current scripts contain absolute paths from the original development environment. Before running, update file path references to point to the cloned repository directory (or use relative paths). A future revision will replace hardcoded paths with configurable variables.

---

## Citation

If you use any portion of this replication package in your own research, please cite both the manuscript (above) and this repository

> Banhe, R. and Francisco, E. de R. *Replication package for "The Immobilism Cascade How AI Initiatives Stall Between Pilot and Production"*, GitHub, 2026, https//github.com/romulo-banhe/mpgc.

---

## License

Code and analytical scripts are released under the MIT License.

Documentation and analytical outputs (codebook, co-occurrence matrix, frequency tables) are released under Creative Commons Attribution 4.0 (CC BY 4.0).

The dissertation, ethical documents (TCLE, Parecer, Instrumento) and the anonymized text remain under the authors' copyright; non-commercial academic reuse is permitted with attribution. Commercial use requires written permission.

---

## Português

### Pacote de Replicação — A Cascata do Imobilismo

Este repositório acompanha o trabalho aplicado de mestrado *"Dados que constroem ou destroem sua estratégia de inteligência artificial"* (FGV-EAESP, MPGC, 2026) e o manuscrito *"The Immobilism Cascade How AI Initiatives Stall Between Pilot and Production"*, em revisão na *MIS Quarterly Executive*.

O repositório contém dados anonimizados, scripts analíticos e documentos de suporte que permitem a outros pesquisadores reproduzir a triangulação qualitativo-quantitativa relatada no manuscrito e inspecionar a trilha de auditoria metodológica.

### Declaração ética

Esta pesquisa foi conduzida sob o protocolo de ética da FGV **P.310.2025** (aprovado em 25 de julho de 2025; ver `Parecer.pdf`). Cada um dos 14 gestores seniores de TI entrevistados assinou um termo de consentimento livre e esclarecido (`Termo de Consentimento_v1.pdf`) que garantiu confidencialidade de suas identidades e acesso restrito às transcrições integrais.

Em conformidade com o termo de consentimento

- **As transcrições integrais das entrevistas não estão incluídas** intencionalmente neste repositório público. Elas estão armazenadas com acesso restrito conforme o compromisso ético assumido com os participantes.
- Todos os artefatos quantitativos neste repositório (codebook, matriz de frequência código-documento, rede de coocorrência, curva de saturação, tabelas de proposição) referenciam os participantes apenas por identificadores anonimizados (E1 a E14).
- O único arquivo textual (`bloco1_anonymized.txt`) usado como input para análise de frequência de termos foi sistematicamente anonimizado (62 substituições cobrindo instituições, empregadores, anos específicos e outros marcadores identificáveis).
- O log de consentimentos da Qualtrics (`respostas_qualtrics.xlsx`) foi redigido para remover endereços IP, geolocalização, identificadores de resposta, nomes de destinatários, emails e timestamps; permanecem apenas a decisão de consentimento e a autorização de gravação.

Pesquisadores interessados nos dados de entrevista subjacentes podem contatar o autor correspondente. Decisões sobre acesso respeitarão os termos originais de consentimento e o protocolo de ética da FGV.

### Contato

Romulo Banhe romulo.banhe@gmail.com
Eduardo de Rezende Francisco eduardo.francisco@fgv.br
