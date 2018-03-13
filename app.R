library(shiny)
library(shinydashboard)
library(exams)
library(DT)
library(stringr)
source("helpers.R")
# Carregar exercícios dentro da variável dados

header <- dashboardHeader(title = strong("Projeto Exams"))

# Menu lateral -----------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Início", tabName = "inicio", icon = icon("comment")),
    menuItem(
      "Pasta pessoal",
      tabName = "pessoal",
      icon = icon("folder-open")
    ),
    # menuItem(
    #   "Criar exame",
    #   tabName = "criar",
    #   icon = icon("plus")
    # ),
    # menuItem(
    #   "Criar questões",
    #   tabName = "criar_questoes",
    #   icon = icon("check-circle")
    # ),
    menuItem("Criação",
             icon = icon("plus"),
             menuSubItem("Exames", "criar"),
             menuSubItem("Questões", "criar_questoes")
    ),
    menuItem(
      "Tutorial do projeto",
      icon = icon("file-code-o"),
      href = "https://gianlima.github.io/r-exams-tutorial/"
    ),
    menuItem(
      "Estatística",
      tabName = "est",
      icon = icon("address-book")
    ),
    menuItem("Configurações", tabName = "config", icon = icon("cog"))
    
  ),
  box(
    width = NULL,
    background = "black",
    "Usuário conectado: ",
    textOutput(outputId = 'user')
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
# conteúdo aba Início -------------------------
    tabItem(tabName = "inicio",
            fluidRow(
              box(width = 12,title = h2(align="center",style = "font-family: 'Helvetica'; font-si16pt",
                                        'Projeto Criação de Exames com pacote Exams')
                  , background = "navy", 
                  
                  h4('Projeto de extensão implementado pelo aluno', strong('Gian de Lima Santos'), 'com orientação
                     do professor', strong('Fernando Mayer.'), align="justify" , 'O trabalho tem como objetivo
                     criar uma interface gráfica', em('user-friendly'), ' para criação de exames com questões 
                     pré-montadas.  Isso é possível com o auxílio de dois pacotes 
                     da linguagem R: ',
                     strong(' Shiny e Exams.'),
                     br(),
                     h4('Com o pacote',strong('Shiny'),'é montado a interface e com o pacote',
                        strong('Exams'),'é realizado tarefas como:'),
                     tags$ul(
                       tags$li(h4("Ordem aleatória das alternativas de cada questão")), 
                       tags$li(h4("Correção automática dos exames")), 
                       tags$li(h4("Criação dos exames em formato PDF pronto para impressão"))
                     )
                  ))
              
              
            )
    ), # fim inicio
    
# conteúdo aba Questões prontas -------------------------
    tabItem(tabName = "criar",
            fluidRow(
              box(width = 12, h3(align="center",style = "font-family: 'Helvetica'",
                                 'Crie seus exames!')
                  , background = "navy") 
            ), # primeira linha
            fluidRow(
              column(width=5,
                     box(width=NULL,
                       title = "1. Selecione o tipo de exercício", status = "warning", solidHeader = TRUE,
                       selectInput("select", label = NULL, 
                                   choices = list("schoice" = "extype: schoice", 
                                                  "mchoice" = "extype: mchoice", 
                                                  "num" = "extype: num",
                                                  "cloze" = "extype: cloze",
                                                  "string" = "extype: string",
                                                  "pessoais" = "usuario",
                                                  "todos exs." = "extype:"), 
                                   selected = "extype: schoice")
                     ),
                     box(width=NULL,
                       title = "3. Descrição do exercício selecionado", status = "success", solidHeader = TRUE,
                       textOutput("descri")
                     ),
                     box(width=NULL,
                       title = "4. Exercício já selecionado", status = "danger", solidHeader = TRUE,
                       # textOutput("descri"),
                       # h4('Com o exercício selecionado:'),
                       actionButton("botao", label = "Visualizar questão"),
                       br(),br(),
                       uiOutput("pdfview"),
                       actionButton("botaoInserir", label = 'Inserir no exame'),
                       br(),br(),
                       # actionButton("botaoExcluirDoExame", label = 'Excluir do exame'),
                       # br(),br(),
                       verbatimTextOutput("showExsInserted"),
                       actionButton("botaoCriarExame", label = 'Criar exame'),
                       br()
                     )
              ),
              column(width=7,
                     box(width=NULL,
                       title = "2. Selecione um dos exercícios abaixo", status = "primary", solidHeader = TRUE,
                       DT::dataTableOutput('opcao_exercicios')
                     )  
              )
            ) # segunda linha
    ), # fim criar
# conteúdo aba Estatística -------------------------
    tabItem(tabName = "est",
            fluidRow(
              
              box(width = 12, background = "navy", 
                  h3(align="center",style = "font-family: 'Helvetica'; font-si16pt",
                     'Estatística'))
            ),
            fluidRow(
              valueBoxOutput("valuebox3"),
              valueBoxOutput("valuebox4"),
              valueBoxOutput("valuebox2"),
              valueBoxOutput("valuebox1")
            ) # segunda row
            
    ),
# conteúdo aba Configurações -------------------------
    tabItem(tabName = "config",
              fluidRow(
                box(width = 12, background = "navy", 
                    h3(align="center",style = "font-family: 'Helvetica'; font-si16pt",
                       'Configurações'))
              ),
              fluidRow(
                box(title = "Utilizar outra conta",
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, width= 3,collapsed = TRUE,
                  actionButton('trocarUser', 'Trocar usuário')
                ),
                # box(title = "Suas pastas pessoais",
                #     status = "primary", solidHeader = TRUE,
                #     collapsible = TRUE, width= 3,
                #     actionButton('visualizarPastas', 'Visualizar pastas'),
                #     textOutput("pastasPessoais")
                # ),
                box(title = NULL,
                    status = "primary", solidHeader = TRUE,
                    collapsible = FALSE, width= 3,#collapsed = TRUE,
                    actionButton('excluirConta', 'Excluir conta')
                )
              )
            ),
# conteúdo aba Criar Questões -------------------------
    tabItem(tabName = "criar_questoes",
            fluidRow(
              box(width = 12, h3(align="center",style = "font-family: 'Helvetica'",
                                 actionLink("hhhhh","Crie seu exercício!"))
                  , background = "navy") 
            ),
            fluidRow(
              box(width =  6,status = "primary",title = "Código R",
                  textAreaInput('codigoInput', NULL, rows = 4,
                                resize = "vertical"
                  )
              ),
              box(width = 6,status = "success",
                  textAreaInput('questionInput', h4('Questão*'), rows = 4,
                                placeholder = 'É possível inserir escrita LaTeX ($$ x^2 $$) ou escrita Rmarkdown (`r var`).' ,
                                resize = "vertical"
                  )
              )
              ),
            fluidRow(
              box(width = 6,status = "warning", title = 'Alternativas',
                  textAreaInput('answerlistInput1', NULL, placeholder = "Cada alternativa deve iniciar com '#' e para ir para a próxima alternativa, pule de linha.", rows = 4,
                                resize = "vertical"
                  )
              ),
              box(width = 3,status = "danger",title = 'Alternativa(s) correta(s):',
                  textInput('exsolution', label = NULL)
              ),
              box(width = 3,status = "warning",
                  numericInput("exshuffle", min = 2, max = 10, step = 1, label = h4("Quantas alternativas mostrar"), value = 5)
              )
            ),
            fluidRow(
              box(width = 6,status = "info",
                                radioButtons("radioAnswer", label = h4("Visualizar solução:"),
                                             choices = list('Indicar apenas como verdadeiro ou falso' = 1, "Personalizar com alternativas comentadas" = 2),
                                             selected = 1),
                                conditionalPanel("input.radioAnswer==2",
                                                 textAreaInput('answerlistInput2', h4('Solução das alternativas comentadas'), rows = 4,
                                                               placeholder = '# Verdadeiro. Curitiba é uma cidade. (aperte enter) # Falso. Paraná não é uma ciadade.' ,
                                                               resize = "vertical"))    
              ),
              box(width = 3,status = "success",
                  textAreaInput('exnameInput', h4('Descrição simples do exercício'), rows = 4,
                                placeholder = 'Apenas para reconhecer o exercício de forma rápida' ,
                                resize = "vertical"
                  )
              ),
              box(width = 3,status = "danger",
                  textAreaInput('solutionInput', h4('Comentário sobre a questão'), rows = 4,
                                placeholder = 'Você pode apenas fazer um comentário ou também mostrar a solução' ,
                                resize = "vertical"
                  )
              )
            ),
            fluidRow(
              box(width = 6,status = "warning",title = "Nome do arquivo",
                  textInput('nomeDoEx', label = NULL, placeholder = 'Sem espaço, sem extensão'),
                  uiOutput("jaExisteEsseExercicio"),
              footer = actionButton('criarEx', 'Criar questão', icon("plus"),
                                    style = "color: #fff; background-color: #337ab7"))
            )
    )
  )
)

ui <- dashboardPage(skin = "black",
                    header,
                    sidebar,
                    body)

server <- function(input, output) {
  user <<- FALSE
  
  observeEvent({ 
    input$hhhhh
  },{
    showModal(modalDialog(size = 'l',
      title = "Ajuda: Código R",
      uiOutput("html"),
      footer = tagList(
        modalButton('OK'))
    ))
  })
  
  output$html <- renderUI({ 
  tags$iframe(style = "height:900px; width:100%", src =
                "ajuda.html")})
  
  observeEvent(input$nomeDoEx,{
    if(file.exists(paste0('usuarios/',user,'/EXS/',input$nomeDoEx,'.Rmd'))){
      output$jaExisteEsseExercicio <- renderUI({ h4(div(tags$i("Já existe um exercício criado com o mesmo nome. 
                               Se continuar com esse nome, o exercício será sobreposto.", 
                               style = "color: red;")))})
    } else{
      output$jaExisteEsseExercicio <- renderUI({ '' })
    }
  })
  
  observeEvent(input$criarEx,{
    
    if(input$radioAnswer == 1){
      qualAnswerlist <- 1
    }
    else{qualAnswerlist <- 2}
    resposta <- criarExer(input$codigoInput,input$questionInput,input$answerlistInput1,input$solutionInput,
              input$answerlistInput2,input$exnameInput,input$exsolution,
              input$exshuffle,input$nomeDoEx,user,qualAnswerlist)
    
    loadData(input$userName,'não')
    a <- dados$arquivo[grepl(input$select, dados$conteudo)]
    a <- gsub('^.*/\\s*|\\s*.Rmd.*$', '', a)
    exercicios <- as.data.frame(a)
    names(exercicios) <- 'Exercicios'
    output$opcao_exercicios <-  DT::renderDataTable(exercicios, server = FALSE,selection = 'single')
    
    if(resposta[1]){
      showNotification(resposta[2], type= "message")
    }
    else{
      showNotification(resposta[2], type= "error")
    }
    
  })
  
  observeEvent(input$excluirConta,{
    showModal(modalDialog(
      size = 's',
      title = h4('Excluir Conta'),
      h4('Tem certeza que deseja excluir sua conta?'),
      h5('Caso deseje fazer o download de arquivos antes, vá em "Pasta Pessoal".'),
      footer = tagList(
        modalButton('Cancelar'),
        actionButton('simExcluirConta', 'Excluir Conta', icon("sign-in"),
                     style = "color: #fff; background-color: #ff0000")
      )
    ))
  })
  
# Evento botão 'confirmar excluir conta pessoal' ------------
  observeEvent(input$simExcluirConta,{
    retorno <-  excluirContaPessoal(user)
    if(retorno[1]){
      showNotification(retorno[2], type = "message")
      user <<- FALSE 
      removeModal()
      bemVindo()
    } else{
      showNotification(retorno[2], type = "error")
    }
  })
  
  output$user <- renderText({
    if (user == F) {
      'Nenhum'
    }
  })
  
# Função conteúdo Modal Inicial ------------------------
  bemVindo <- function(){
    showModal(modalDialog(
      size = 's',
      title = strong("Bem-vindo ao Projeto Exams"),
      textInput("userName", label = h4("Usuário cadastrado")),
      footer = tagList(
        actionButton('criarConta', 'Criar usuário', icon("list-ul")),
        actionButton('conectar', 'Conectar', icon("sign-in"),
                     style = "color: #fff; background-color: #337ab7")
      )
    ))
    
  }
  
# Modal Inicial  ------------------------
  
  bemVindo()

  observeEvent(input$voltarBemVindo, {
    removeModal()
    bemVindo()
  })
  
  observeEvent(input$trocarUser, {
    bemVindo()
  })
  
  # botão CONECTAR da tela de bem-vindo
  observeEvent(input$conectar
               , {
                 if (input$userName == "") {
                   showNotification("Insira nome de usuário cadastrado", type = "error")
                 }
                 if (any(trimws(input$userName) == list.files('usuarios'))) {
                   removeModal()
                   user <<- input$userName
                   loadData(input$userName,'sim')
                   output$user <- renderText({
                     input$userName
                   })
                 } else {
                   showNotification("Usuário não encontrado.
                                    ", type = "error")
                 }
                 
                 output$valuebox1 <- renderValueBox({
                   valueBox(length(list.files(paste0(
                     'usuarios/', user
                   ))),
                   'PASTAS CRIADAS POR MIM',
                   icon = icon("check-circle"),
                   color = "purple")
                 })
                 
                 output$valuebox2 <- renderValueBox({
                   valueBox(
                     length(
                       list.files(
                         path = "usuarios",
                         pattern = ".pdf$",
                         recursive = TRUE
                       )
                     ),
                     "EXAMES INDIVIDUAIS",
                     icon = icon("file"),
                     color = "lime"
                   )
                 })
                 
                 output$valuebox3 <- renderValueBox({
                   valueBox(
                     length(list.files("usuarios")),
                     "USUÁRIOS DO PROJETO",
                     icon = icon("users"),
                     color = "yellow"
                   )
                 })
                 
                 output$valuebox4 <- renderValueBox({
                   valueBox(
                     length(
                       list.files(
                         path = "usuarios",
                         pattern = ".rds$",
                         recursive = TRUE
                       )
                     ),
                     "CONJUNTOS DE EXAMES",
                     icon = icon("folder-open"),
                     color = "red"
                   )
                 })
                 
                 #output$showExsInserted <- renderText({'Insira exercícios.'})
                 
                 output$pastasPessoais <- renderText({
                   list.files(path = paste0('usuarios/', user))
                 })
                 a <- dados$arquivo[grepl(input$select, dados$conteudo)]
                 a <- gsub('^.*/\\s*|\\s*.Rmd.*$', '', a)
                 exercicios <- as.data.frame(a)
                 names(exercicios) <- 'Exercicios'
                 output$opcao_exercicios <-  DT::renderDataTable(exercicios, server = FALSE,selection = 'single')
               })
  
  # botão CRIAR CONTA da tela de bem-vindo
  observeEvent(input$criarConta, {
    removeModal()
    showModal(
      modalDialog(
        size = 'm',
        title = strong("Criar cadastro"),
        textInput("userNameNew", label = h4("Novo usuário (sem espaços)")),
        passwordInput("password", h4("Senha do projeto")),
        footer = tagList(
          actionButton('criarContaComSenha', 'Criar Conta', icon("plus"),
                       style = "color: #fff; background-color: #337ab7"),
          actionButton('voltarBemVindo', 'Voltar', icon("undo"))
        )
      )
    )
  })
  
  observeEvent(input$criarContaComSenha, {
    if (input$userNameNew == "") {
      showNotification("Insira nome de usuário", type = "error")
    }
    if (input$password != 'exames') {
      if (any(trimws(input$userNameNew) == list.files('usuarios'))) {
        showNotification("Senha incorreta", type = "error")
        showNotification("Usuário já existente", type = "error")
      } else {
        showNotification("Senha incorreta", type = "error")
      }
    } else {
      if (any(trimws(input$userNameNew) == list.files('usuarios'))) {
        showNotification("Usuário já existente", type = "error")
      }
      else {
        if (input$userNameNew != "") {
          dir.create(paste0('usuarios/', trimws(input$userNameNew)))
          dir.create(paste0('usuarios/',trimws(input$userNameNew),'/EXS'))
          showNotification("Conta criada", type = "message")
          removeModal()
          user <<- input$userNameNew
          output$user <- renderText({
            trimws(input$userNameNew)
          })
          showNotification("Faça login", type = "message")
          bemVindo()
        }
      }
    }
    
    
  })
  
  
  observeEvent(input$select, {
    
    if(input$select != 'usuario'){
      a <- dados$arquivo[grepl(input$select, dados$conteudo)]
      a <- gsub('^.*/\\s*|\\s*.Rmd.*$', '', a)
      exercicios <- as.data.frame(a)
    } else{
      a <- dados$arquivo[grepl('usuarios', dados$arquivo)]
      a <- gsub('^.*/\\s*|\\s*.Rmd.*$', '', a)
      exercicios <- as.data.frame(a)
    }
    names(exercicios) <- 'Exercicios'
    output$opcao_exercicios <-  DT::renderDataTable(exercicios, server = FALSE,selection = 'single')
  }) # end observeEvent
  
  
  
  df <- eventReactive(input$botao, {
    # criatePDF(input$insertEx)
    
    if(file.exists(paste0('exercicios/',input$opcao_exercicios_cell_clicked$value,'.Rmd'))){
      if (criatePDF(paste0('exercicios/',input$opcao_exercicios_cell_clicked$value,'.Rmd'))) {
        showModal(
          modalDialog(
            size = 'l',
            title = h4(paste0('Visualização do exercício ',input$opcao_exercicios_cell_clicked$value)),
            tags$iframe(style = "height:700px; width:100%", src =
                          "plain81.pdf"),
            easyClose = TRUE,
            footer = tagList(modalButton("OK"))
          )
        )
      }
    } else if(file.exists(paste0('usuarios/',user,'/EXS/',input$opcao_exercicios_cell_clicked$value,'.Rmd'))){
      if (criatePDF(paste0('usuarios/',user,'/EXS/',input$opcao_exercicios_cell_clicked$value,'.Rmd'))) {
        showModal(
          modalDialog(
            size = 'l',
            title = h4(paste0('Visualização do exercício ',input$opcao_exercicios_cell_clicked$value)),
            tags$iframe(style = "height:600px; width:100%", src =
                          "plain81.pdf"),
            easyClose = TRUE,
            footer = tagList(modalButton("OK"))
          )
        )
      }
    } else{
      showNotification('Selecione um exercício primeiramente',type = "error")
      ''
    }
    
    
  })
  
  # observeEvent(input$insertEx, {
  #   output$descri <- renderText({ getDesc(input$insertEx) })
  # })

  output$descri <- renderText({ 
      ex <- input$opcao_exercicios_cell_clicked$value
      if(length(ex)!= 0){
        if(any(dados$arquivo == paste0('exercicios/',ex,'.Rmd'))){
          dados$desc[(dados$arquivo == paste0('exercicios/',ex,'.Rmd'))]
        } else {
          dados$desc[(dados$arquivo == paste0('usuarios/',user,'/EXS/',ex,'.Rmd'))]
        }
      }
      else {
        'Obs.: Selecione um exercício da tabela para visualizar a descrição.'
      }
    })
  
  output$oqsera <- renderText({ 
    input$opcao_exercicios_cell_clicked$value
    })
  
  output$pdfview <- renderUI({ 
    df()
  })
  
  funcButtonInserEx <- eventReactive(input$botaoInserir,  {
    a <-  input$opcao_exercicios_cell_clicked$value
    if(length(a)!=0){
      if(filedAddedAlready(input$opcao_exercicios_cell_clicked$value)){
        # showNotification(paste0('O exercício ',
        #                         input$opcao_exercicios_cell_clicked$value,
        #                         ' já foi adicionado!'),
        #                  type = "error")
        showNotification(paste0('Exercício ',
                                gsub('^.*/\\s*|\\s*.Rmd.*$', 
                                     '', 
                                     input$opcao_exercicios_cell_clicked$value),
                                ' já foi adicionado!'),type = 'error')
        as.character(gsub('^.*/\\s*|\\s*.Rmd.*$', '', exerciciosExame))
      } else{
        insertInList(input$opcao_exercicios_cell_clicked$value)
      }
    } else {
      showNotification('Selecione um exercício.',type = 'error')
      as.character(gsub('^.*/\\s*|\\s*.Rmd.*$', '', exerciciosExame))
    }
    as.character(gsub('^.*/\\s*|\\s*.Rmd.*$', '', exerciciosExame))
    
  })
  
  # observeEvent(input$botaoExcluirDoExame,  {
  #   if(length(exerciciosExame)!=0){
  #     exerciciosExame[exerciciosExame == input$opcao_exercicios_cell_clicked$value] <<- NULL
  #   } else{
  #     showNotification('Não há exercício para excluir!',type = "error")
  #   }
  #   output$showExsInserted <- renderText({as.character(exerciciosExame)})
  #   
  # })
  
  output$showExsInserted <- renderText({
    
    funcButtonInserEx()
    
  })
  
  observeEvent(input$botaoCriarExame,  {
    if(length(exerciciosExame) == 0) {
      showModal(modalDialog(
        title = h4("Atenção!"),
        h4(div(tags$i("Nenhum exercício foi adicionado no exame. Insira exercício(s) antes de prosseguir.", style = "color: red;"))),
        easyClose = TRUE,
        footer = tagList(
          modalButton("OK")
        )
      ))
    } else {
      showModal(modalDialog(size = 's',
        title = "Insira dados do exame a ser criado.",
        textInput("institution", label = h4("Instituição"), value = "UFPR"),
        textInput("disciplinaNome", label = h4("Nome da disciplina"), 
                  value = "Introdução à Computação"),
        textInput("disciplinaSigla", label = h4("Código da disciplina"), value = "CE083"),
        textInput("qntExames", label = h4("Quantidade de exames"), value = "2"),
        radioButtons("radio", label = h4("Valor de cada questão"),
                     choices = list('Todas questões valerão a mesma nota' = 1, "Inserir valores diferentes" = 2),
                     selected = 1),
        conditionalPanel("input.radio==2",
                         textInput("pontos", label = h4(paste0('Pontos de cada questão, inserir ',length(exerciciosExame),' valor(es)')), placeholder = "2,2,3,4")),
        conditionalPanel("input.radio==1",
                 h5(strong(paste0('Obs.: Cada questão valerá ',round(10/length(exerciciosExame), digits = 2),' pontos')))),        

        dateInput("date",language = "pt-BR", label = h4("Data do exame"), value = Sys.Date()),
        textInput("pasta", label = h4("Pasta para salvar exames"), value = "",placeholder = "pasta"),
        uiOutput("logicalPasta1"),
        footer = tagList(
          modalButton("Voltar", icon("undo")),
          actionButton("ok", "Criar exames!")
        )
      ))
    }
  })
  
  observeEvent(input$pasta,  {
    if(dir.exists(paste0('usuarios/',user,'/',input$pasta)) && input$pasta != "" && input$pasta != 'EXS'){
      output$logicalPasta1 <- renderUI({ h4(div(tags$i("Você já criou esta pasta. Caso queira
        continuar com o mesmo nome, a pasta antiga será sobreposta pela nova.", style = "color: red;"))) })
    } else{
      if(input$pasta == "EXS"){
        output$logicalPasta1 <- renderUI({ h4(div(tags$i("Nome reservado para o sistema. Escolha outro nome.", style = "color: red;"))) })
      } else{
        output$logicalPasta1 <- renderUI({ '' })
      }
    }
  })
  
  observeEvent(input$ok,  {
    
    if(input$pasta == ""){
      showNotification('Insira nome na pasta',type = 'error')
    } else if(input$pasta == "EXS"){
      showNotification('Troque o nome da pasta!',type = 'error')
    } else if(input$radio==2 && input$pontos == ""){
      showNotification('Insira pontuação.',type = 'error')
    } else {
      removeModal()
      
      showModal(modalDialog(
        title = "Criando os exames",
        "Exame está sendo criado... (espere alguns segundos)",
        footer = NULL
      ))
      
      if(input$radio==1){
        qntQuestoes <- length(exerciciosExame)
        valorCadaQuestao <- round(10/qntQuestoes,digits = 2)
        qual <- valorCadaQuestao
        for(i in rep(valorCadaQuestao,qntQuestoes-1) ){
          qual <- paste(qual,i,sep = ",")
        }
        if (class(qual) == "numeric"){
          qual <- as.character(qual)
        }
        if(createExams(input$qntExames,input$institution,
                       input$disciplinaSigla,
                       input$disciplinaNome,qual,input$date,input$pasta)){
          removeModal()
          showModal(modalDialog(
            title = "Exame criado!",
            paste0('Exame criado com sucesso na pasta ',input$pasta,'.'),
            br(),
            footer = tagList(
              downloadButton("downloadData", 'Download'),
              modalButton("Fechar", icon("remove", lib = "glyphicon"))
            )
          ))
        }
      }
      if(input$radio==2){
        if(createExams(input$qntExames,input$institution,
                       input$disciplinaSigla,
                       input$disciplinaNome,input$pontos,input$date,input$pasta)){
          removeModal()
          showModal(modalDialog(
            title = "Exame criado!",
            paste0('Exame criado com sucesso na pasta ',input$pasta,'.'),
            br(),
            footer = tagList(
              downloadButton("downloadData", 'Download'),
              modalButton("Fechar", icon("remove", lib = "glyphicon"))
            )
          ))
        }
      }
    }
    
    

    
  })
  
  # observeEvent(input$trocarNomePasta,  {
  #   
  #   showModal(modalDialog(
  #     title = "Insira dados do exame a ser criado.",
  #     textInput("institution", label = h4("Instituição"), value = "UFPR"),
  #     textInput("disciplinaNome", label = h4("Nome Disciplina"), 
  #               value = "Introdução à Computação"),
  #     textInput("disciplinaSigla", label = h4("Sigla Disciplina"), value = "CE083"),
  #     textInput("qntExames", label = h4("Quantidade de exames"), value = "8"),
  #     textInput("pontos", label = h4("Pontos de cada questão"), value = "2,3,3,2"),
  #     dateInput("date", label = h3("Date input"), value = Sys.Date()),
  #     textInput("pasta", label = h4("Pasta para salvar exames:"), value = "CE083"),
  #     textOutput("logicalPasta"),
  #     footer = tagList(
  #       modalButton("Voltar"),
  #       actionButton("ok", "Criar exames!")
  #     )
  #   ))
  # })
  
  # observeEvent(input$pasta,  {
  #   if(dir.exists(paste0('usuarios/',user,'/',input$pasta))){
  #     output$logicalPasta <- renderText({ 'Pasta já existe, caso continue com este nome
  #       a pasta será sobreposta.' })
  #   } 
  # })
  
  # observeEvent(input$continuarPasta,  {
  #   removeModal()
  #   
  #   showModal(modalDialog(
  #     title = "Criando os exames",
  #     "Exame está sendo criado... (espere alguns segundos)",
  #     footer = NULL
  #   ))
  #   
  #   if(createExams(input$qntExames,input$institution,
  #                  input$disciplinaSigla,
  #                  input$disciplinaNome,input$pontos,input$date,input$pasta)){
  #     removeModal()
  #     showModal(modalDialog(
  #       title = "Exame criado!",
  #       paste0('Exame criado com sucesso na pasta ',input$pasta,'.'),
  #       br(),
  #       footer = tagList(
  #         downloadButton("downloadData", 'Download'),
  #         modalButton("Fechar", icon("remove", lib = "glyphicon"))
  #       )
  #     ))
  #   }
  # })
  
  
  output$downloadData <-  downloadHandler(
    filename = function() {
      nome.arquivo.baixar <- paste0(input$pasta, '.zip')
    },
    content = function(file) {
      zipout <- paste0(nomeZip, '.zip')
      if (!file.exists(zipout))
        stop('Error! cant find file!')
      file.copy(zipout, file)
    },
    contentType = "application/zip"
  )
  
  exerciciosExame <<- list()
  
}

shinyApp(ui, server)

