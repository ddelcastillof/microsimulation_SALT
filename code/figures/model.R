#--------------------------------
# Microsimulation model structure
#--------------------------------

# Load libraries
library(DiagrammeR)
library(xml2)
library(here)

# Create the model structure diagram
model_diagram <- grViz("
                    digraph{
                    rankdir=LR;

                    graph[ranksep=0.2]
                    
                    node[shape=plaintext, fontname = 'Arial']
                    
                    # Define nodes
                    Start [label='Healthy', shape=oval]
                    HTA [label='Hypertension', shape=box]
                    CVD [label='Cardiovascular Disease', shape=box]
                    Final [label='Death', shape=oval]

                    edge[minlen=2]
                    # Define transitions
                    Start -> HTA [label='p1']
                    Start -> CVD [label='p2']
                    Start -> Final [label='p3']

                    HTA -> CVD [label='p4']
                    HTA -> Final [label='p5']

                    CVD -> Final [label='p6']

                    Final -> Final [label='1']

                    # Rank the nodes
                    {rank=min; Start}
                    {rank=same; HTA}
                    {rank=same; CVD}
                    {rank=max; Final}
                    }
                    "
)

print(model_diagram)

model_diagram |>
  DiagrammeRsvg::export_svg() |>
  read_xml() |>
  write_xml(here::here("figs", "model_structure.svg"))


# Create DAG for causal assumptions
dag <- grViz("
              digraph{
                  graph[ranksep=0.2]
                    
                  node[shape=plaintext, fontname = 'Arial']
                    
                  # Define nodes
                  Interv [label='Salt', shape=box]
                  HTA [label='High BP', shape=oval]
                  CVD [label='CVD', shape=oval]
                  Age [label='Age', shape=oval]
                  SES [label='SES', shape=oval]
                  Fam [label='Fam. hist.', shape=oval]
                  Educ [label='Educ. lvl', shape=oval]

                  edge[minlen=2]
                  # Define transitions
                  Interv -> HTA
                  Interv -> CVD
                  
                  Age -> HTA, CVD
                  
                  SES -> HTA, CVD

                  Fam -> HTA, CVD

                  Educ -> HTA, CVD, SES

                  HTA -> CVD

                  # Rank the nodes
                  {rank=min; Interv, HTA, CVD}
                  {rank=max; Age, SES, Fam, Educ}
              }     
              "
)

print(dag)

dag |>
  DiagrammeRsvg::export_svg() |>
  read_xml() |>
  write_xml(here::here("figs", "causal_dag.svg"))