# Banco de Dados Epidemiológicos de Araruama (RJ)

Repositório de dados e indicadores de saúde do município de **Araruama (RJ)**, construído a partir de fontes oficiais do DATASUS e do IBGE.

- **Código IBGE:** 330020 (6 dígitos) / 3300209 (7 dígitos)
- **População (Censo 2022):** 129.671 habitantes
- **Tamanho do banco:** ~7 MB (Parquet, altamente comprimido)
- **Período coberto:** 1996–2024 (varia por sistema)

---

## 📊 Fontes de dados

| Sistema | Descrição | Período | Fonte |
| :--- | :--- | :--- | :--- |
| **SIM** | Sistema de Informação sobre Mortalidade | 1996–2024 | DATASUS |
| **SINASC** | Sistema de Informação sobre Nascidos Vivos | 1996–2024 | DATASUS |
| **SINAN** | Sistema de Informação de Agravos de Notificação (dengue) | 2007–2023 | DATASUS |
| **População** | Estimativas populacionais | 2000–2024 | IBGE (`brpop`) |
| **CID-10** | Classificação Internacional de Doenças | — | `cid10` (GitHub) |
| **Prévia TabNet** | Dados preliminares de mortalidade | 2025–2026 | DATASUS |

---

## 📁 Estrutura do projeto

    BancoEpidemio/
    ├── bruto/                          # Camada 1 — dados originais filtrados
    │   ├── sim/                        # SIM-DO (óbitos), 29 arquivos
    │   ├── sim_preliminar/             # Prévia TabNet 2025-2026
    │   ├── sinasc/                     # SINASC (nascidos vivos), 29 arquivos
    │   ├── populacao/                  # População IBGE 2000-2024
    │   └── sinan/
    │       └── dengue/                 # SINAN dengue 2007-2023, 17 arquivos
    │
    ├── tratado/                        # Camada 2 — dados limpos
    │   ├── fato_obitos.parquet                (26.311 óbitos × 103 colunas)
    │   ├── fato_obitos_redistribuido.parquet  (26.303 óbitos redistribuídos)
    │   ├── fato_nascimentos.parquet           (46.356 nascimentos)
    │   └── fato_dengue.parquet                (7.286 casos de dengue)
    │
    ├── analytics/                      # Camada 3 — indicadores prontos
    │   ├── ind_*.parquet              # 23 tabelas de indicadores
    │   └── grafico_*.png              # 17 gráficos prontos
    │
    ├── scripts/                        # Pipeline reprodutível
    │   ├── 00_setup.R
    │   ├── 01_utils.R
    │   ├── 02-08_*.R                  # SIM
    │   ├── 10-12_*.R                  # SINASC
    │   ├── 20-22_*.R                  # SINAN dengue
    │   └── 99_status.R                # Inventário do banco
    │
    ├── dashboard/                      # Dashboard Shiny (em desenvolvimento)
    │   ├── global.R
    │   ├── ui.R
    │   ├── server.R
    │   ├── app.R
    │   └── www/logo.png
    │
    ├── run_all.R                       # Orquestrador do pipeline
    └── README.md

---

## 🚀 Como reproduzir o banco

### Pré-requisitos

Sistema testado: **CachyOS/Arch Linux** com **R 4.6.1**, locale `pt_BR.UTF-8`.

Instale os pacotes R:

    install.packages(c(
      "shiny", "bslib", "plotly", "arrow", "dplyr", "tidyr",
      "stringr", "ggplot2", "datasus", "microdatasus",
      "healthbR", "brpop", "remotes", "lobstr"
    ))
    
    # cid10 (removido do CRAN, instalar do GitHub)
    remotes::install_github("msrodrigues/cid10")

> ⚠️ No Arch/CachyOS, o pacote `arrow` pode falhar ao compilar por limitação de cota do `/tmp`. Solução: `sudo pacman -S arrow` e depois `Sys.setenv(ARROW_USE_PKG_CONFIG = "TRUE"); install.packages("arrow")`.

### Execução

**Reproduzir todo o banco do zero:**

    source("run_all.R")
    run_all()

**Atualizar apenas o que está pendente (uso diário):**

    source("run_all.R")
    run_all(apenas_pendentes = TRUE)

**Ver o inventário atual:**

    source("scripts/99_status.R")

O pipeline é **retomável**: cada script verifica se sua saída já existe e pula se estiver completa. Se a internet cair no meio, basta rodar de novo.

### Tempo estimado (do zero)

| Sistema | Tempo |
| :--- | ---: |
| SIM (29 anos) | ~30–60 min |
| População | ~5 s |
| SINASC (29 anos) | ~3–5 min |
| SINAN dengue (18 anos) | ~30 min |
| **Total** | **~1–2 h** |

---

## ⚠️ Notas metodológicas

### 1. Códigos de município (6 vs 7 dígitos)

Diferentes sistemas e anos usam formatos distintos:
- **SIM 1996** usava 7 dígitos (`3300209`)
- **SIM 1997+** usa 6 dígitos (`330020`)
- **SINASC 1996-2005** usava 7 dígitos; **2006+** usa 6

O pipeline normaliza automaticamente via `normalizar_cod_municipio()`.

### 2. Períodos cobertos

- **Óbitos e nascimentos:** 1996–2024
- **Taxa de mortalidade:** 2000–2024 (população IBGE começa em 2000)
- **Dengue:** 2007–2023 (o SINAN só estrutura dengue a partir de 2007)
- **Prévia:** 2025–2026 (dados preliminares do TabNet)

### 3. Anomalia populacional em 2007

A população caiu de 100.378 (2006) para 98.268 (2007) — improvável. Provavelmente metodológico (revisão pós-Censo 2000). Série mantida como oficial.

### 4. Mudança metodológica em 2022 (Censo)

A população saltou de 136.109 (2021) para 129.671 (2022, Censo real). A queda aparente da taxa em 2022 reflete essa mudança, não a epidemiologia. **Séries pré e pós-Censo não são perfeitamente comparáveis.**

### 5. Anos preliminares

**SIM:** 2023 e 2024 são preliminares e podem ser revisados.
**SINASC:** 2024 é preliminar.

### 6. Redistribuição de causas mal definidas (SIM)

Óbitos classificados como causas mal definidas foram redistribuídos proporcionalmente entre causas específicas, estratificando por **ano × sexo × faixa etária**.

Códigos tratados como mal definidos (garbage codes):
- **R99** — Causa de morte não especificada
- **R95** — Morte súbita na infância
- **P95** — Morte fetal de causa não especificada
- **B34** — Infecção viral de localização não especificada (COVID sem confirmação)
- **X59** — Exposição a fator não especificado
- **V89** — Acidente de transporte não especificado

- Total redistribuído: ~2.955 óbitos (11,2% do total)
- Metodologia: redistribuição proporcional intra-estrato
- Referência: adaptado do GBD (Global Burden of Disease), Classes 1 e 2

Ressalva: a redistribuição introduz frações decimais na contagem por causa. Isso é esperado e preserva o total geral.

### 7. APVP — Anos Potenciais de Vida Perdidos

- **Limite:** 70 anos (padrão do Ministério da Saúde)
- **Cálculo:** usa ponto médio da faixa etária como aproximação da idade

### 8. SINASC — Fluxo obstétrico 2017-2019

Entre 2017 e 2019, apenas **~10% dos nascimentos de mães residentes em Araruama ocorreram no próprio município** (contra ~80% em outros anos). Isso indica **migração obstétrica temporária** — possivelmente por reforma/descredenciamento da maternidade local.

Requer investigação junto à Secretaria de Saúde de Araruama.

### 9. SINAN dengue — Anomalia de 2014

2014 teve apenas **14 casos** de dengue, entre dois anos com ~600 e ~510 casos. Estatisticamente anômalo. Pode ser:
- Imunidade de rebanho após epidemia de 2013 (plausível)
- Subnotificação (mais provável)

Requer investigação junto à Vigilância Epidemiológica.

### 10. SINAN dengue — Mudança de classificação (2014)

A partir de 2014, o SINAN passou a usar novos códigos de classificação final:
- **Pré-2014:** códigos "1" (dengue), "2" (com complicações), "3"/"4" (grave)
- **Pós-2014:** códigos "8"/"10" (dengue), "11" (com sinais de alarme), "12" (grave)

O pipeline reconhece todos os códigos historicamente válidos.

### 11. Dengue 2024 — indisponível

O arquivo `.dbc` do SINAN dengue 2024 está **corrompido no servidor do DATASUS** (`blast decompression failed`). Será incluído automaticamente quando o DATASUS republicar.

### 12. Prévia 2025-2026

Dados agregados do TabNet (não microdados). Sujeitos a revisões substanciais. **Não são usados em análises consolidadas** — aparecem separadamente na dashboard com sinalização visual distinta.

---

## 🎯 Indicadores disponíveis

### Mortalidade
- Taxa de mortalidade geral (série histórica 2000–2024)
- Top 10 causas de morte (absoluto)
- Mortalidade por capítulo CID-10
- Anos Potenciais de Vida Perdidos (APVP)
- Top 10 causas por APVP
- Pirâmide de mortalidade por sexo e faixa etária

### Materno-infantil
- Taxa de natalidade
- Mortalidade infantil (neonatal + pós-neonatal)
- Razão de mortalidade materna
- Proporção de baixo peso ao nascer
- Prematuridade
- Apgar 5' < 7
- Tipo de parto (vaginal vs cesáreo)
- Gravidez na adolescência
- Perfil materno por idade

### Dengue
- Incidência anual (por 100.000 hab)
- Série semanal epidemiológica
- Perfil por sexo, faixa etária e raça
- Sazonalidade mensal
- Indicadores de gravidade (sinais de alarme, dengue grave, óbitos)

---

## 🔍 Achados principais

### Mortalidade geral
- **Pico da COVID-19 em 2021:** 1.595 óbitos, taxa de 11,7/1.000 hab (vs. 9,65 em 2024)
- **Infarto agudo do miocárdio (I21)** é a principal causa absoluta de morte
- **Agressão por arma de fogo (X95)** é a principal causa em APVP — **33.624 anos de vida perdidos**, mais que o dobro do segundo colocado

### Materno-infantil
- **Mortalidade infantil caiu ~50% em 28 anos:** de ~20/1.000 NV (1996) para ~10/1.000 (2024)
- **Gravidez na adolescência despencou:** de 24,9% (1996) para 10,9% (2024)
- 🚨 **Taxa de cesárea em 69,5% (2024)** — **4 a 5x acima do ideal** da OMS (10-15%)
- 🚨 **Razão de mortalidade materna ~70/100.000 NV** — mais que o dobro da meta OMS (<30)

### Dengue
- **Duas epidemias claras:** 2008 (1.531/100k) e 2013 (1.478/100k)
- **Pico sazonal em abril** — orienta ação de vigilância em março
- **57% dos casos em mulheres**, concentração em 15-44 anos
- **62% dos casos notificados em outros municípios** — importante para gestão regional

---

## 🔮 Roadmap

Sistemas planejados para expansão do banco:

| Sistema | Descrição | Prioridade |
| :--- | :--- | :---: |
| **SINAN outros agravos** | Chikungunya, zika, tuberculose, hanseníase, meningite, hepatites, sífilis congênita | 🔴 Alta |
| **SIH** | Internações hospitalares (1992–2024) | 🟡 Média |
| **CNES** | Cadastro de estabelecimentos, leitos, equipamentos, profissionais | 🟡 Média |
| **SIA** | Produção ambulatorial | 🟢 Baixa |
| **PNI** | Cobertura vacinal | 🟢 Baixa |

A infraestrutura (scripts, run_all, dashboard) já suporta a adição desses sistemas com o mesmo padrão.

---

## 🛠️ Stack tecnológico

- **Linguagem:** R 4.6.1
- **Aquisição de dados:** `datasus`, `microdatasus`, `healthbR`, `brpop`
- **Armazenamento:** Parquet (`arrow`)
- **Manipulação:** `dplyr`, `tidyr`, `purrr`
- **Visualização:** `ggplot2`, `plotly`
- **Dashboard:** `shiny` + `bslib`
- **Sistema operacional:** CachyOS (Arch Linux)

---

## 📝 Licença e citação

Os dados brutos são de domínio público (DATASUS/IBGE). O código deste repositório pode ser usado livremente, com atribuição.

Para citar:

    Banco de Dados Epidemiológicos de Araruama (RJ). 2026.
    Disponível em: [URL do repositório no GitHub].
    Fontes: DATASUS (SIM, SINASC, SINAN), IBGE (população).

---

## 👤 Autor

Projeto desenvolvido em colaboração entre a equipe de saúde de Araruama (RJ) e o(a) pesquisador(a) responsável.

---

## 🔗 Links úteis

- [DATASUS — TabNet](https://datasus.saude.gov.br/)
- [OpenDataSUS](https://opendatasus.saude.gov.br/)
- [IBGE — Cidades](https://cidades.ibge.gov.br/)
- [MonitoraRJ](https://monitorar.saude.rj.gov.br/)