loadData <- function(user,limpar){
  if(limpar == 'sim'){
    exerciciosExame <<- list() 
  } 
  exs <- as.character(c(list.files('exercicios',full.names = TRUE),
                        list.files(paste0('usuarios/',user,'/EXS'), full.names = TRUE)))
  dados <<- data.frame(arquivo =  exs)
  dados$arquivo <<- as.character(dados$arquivo)
  dados$conteudo <<- lapply(dados$arquivo, colocarConteudo)
  dados$desc <<- gsub('^.*exname:\\s*|\\s*;;.*$', '', dados$conteudo)
}

colocarConteudo <- function(x){
  paste(readLines(x,n = -1,encoding = "UTF-8"),collapse=" ")
}

criatePDF <- function(nomeExe){
  # library(exams)
  # path <- paste0('exercicios/',nomeExe,'.Rmd')
  exams2pdf(nomeExe, dir = "www")
  logical <- file.exists('www/plain81.pdf')
  return (logical)
}

insertInList <- function(exerToInsert){
  
  # if(fileDoesntExist(exerToInsert) == F && filedAddedAlready(exerToInsert) == F){
  exerciciosExame[length(exerciciosExame) + 1] <<- exerToInsert
  # }
  return(as.character(gsub('^.*/\\s*|\\s*.Rmd.*$', '', exerciciosExame)))
}

# fileDoesntExist <- function(exercise){
#   path <- paste0('exercicios/',exercise,'.Rmd')
#   checkIfAllFalse <- dados$arquivos == path
#   if(all(!checkIfAllFalse)){
#     # arquivo NÃO existe?
#     return(TRUE)
#   }
#   else return(FALSE)
# }

filedAddedAlready <- function(exercise){
  if(any(exerciciosExame == exercise)){
    # arquivo já adiconado?
    return(TRUE)
  }
  else return(FALSE)
}

createExams <- function(qntExames,instituicao,disciplinaSigla,
                        disciplinaNome,pontos,data,pasta ){
  qntExames <- as.numeric(qntExames)
  valores <- as.numeric(unlist(strsplit(pontos, ",")))
  # exerciciosListas <- paste0('exercicios/',exerciciosExame,'.Rmd')
  var <- list()
  for(i in exerciciosExame){
    var[length(var)+1] <- dados$arquivo[grepl(paste0('/',i,'.Rmd'), dados$arquivo)]
  }
  exams2nops(var, n = qntExames,
             dir = paste0('usuarios/',user,'/',pasta), name = "demo", date = data,
             points = valores, showpoints = TRUE, language = 'pt',
             title = disciplinaNome,
             institution = instituicao,
             course =  disciplinaSigla,
             reglength = 8, encoding = 'UTF-8', 
             logo = file.path(getwd(), "ufpr.png"))
  createZip(pasta)
  return(TRUE)
}

createZip <- function(pasta){
  caminhoExames <- paste0('usuarios/',user,'/',pasta)
  files2zip <- dir(caminhoExames, full.names = TRUE)
  nomeZip <<- paste0(caminhoExames,'/',pasta)
  zip(zipfile = nomeZip, files = files2zip)
}

excluirContaPessoal <- function(user){
  pasta <- paste0('usuarios/',user)
  if(dir.exists(pasta)){
    unlink(pasta, recursive = TRUE)
    mensagem <- paste0('Conta do usuário ', user,' removida com sucesso. 
                       Sua conta não existe mais.')
    return(c(TRUE,mensagem))
  } else{
    mensagem <- paste0('Usuário ', user,' não encontrado.')
    return(c(FALSE,mensagem))
  }
}

criarExer <- function(codeInput,questionInput,answerlist1Input,solutionInput,
                      answerlist2Input,exnameInput,exsolutionInput,
                      exshuffleInput,nameInput,user,qualAnswerlist){
  
  exs <- unlist(strsplit(list.files('exercicios'), split='.Rmd', fixed=TRUE))
  if(questionInput == "" || trimws(questionInput) == "" ){
    mensagem <- paste0('Insira questão do exercício.')
    return(c(FALSE,mensagem))
  }
  if(answerlist1Input == "" || trimws(answerlist1Input) == "" ){
    mensagem <- paste0('Insira as alternativas do exercício.')
    return(c(FALSE,mensagem))
  }
  if(exsolutionInput == "" || trimws(exsolutionInput) == "" ){
    mensagem <- paste0('Mostre quais são as alternativas corretas')
    return(c(FALSE,mensagem))
  }
  if(any(nameInput == exs)){
    mensagem <- paste0('Nome do exercício reservado para exercícios prontos pelo sistema.')
    return(c(FALSE,mensagem))
  }
  if(!is.numeric(exshuffleInput)){
    mensagem <- paste0('Insira apenas número no exshuffle')
    return(c(FALSE,mensagem))
  }
  if(exshuffleInput<2){
    mensagem <- paste0('Insira um número maior ou igual a 2!')
    return(c(FALSE,mensagem))
  }
  code <- '```{r data generation, echo = FALSE, results = "hide"}'
  codeInput <- paste0(codeInput,'\n```')
  question <- "\nQuestion\n========"
  # questionInput
  answerlist <- "\nAnswerlist\n----------"
  # answerlist1Input
  solution <- "\nSolution\n========"
  # solutionInput
  metaInformation <- "\nMeta-information\n================"
  if(exnameInput == "" || trimws(exnameInput) == "" ){
    mensagem <- paste0('Insira descrição do arquivo.')
    return(c(FALSE,mensagem))
  }
  exnameInput <- paste0("exname: ",exnameInput,';;')
  total <- str_count(answerlist1Input, pattern = "#")
  if((total != str_count(answerlist2Input, pattern = "#")) && qualAnswerlist != 1){
    mensagem <- paste0('Quantidade de resposta e soluções não batem.')
    return(c(FALSE,mensagem))
  }
  if(nameInput == "" || trimws(nameInput) == "" ){
    mensagem <- paste0('Insira nome do exercício.')
    return(c(FALSE,mensagem))
  }
  # exsolutionInput <- '1,5' # será exsolutionInput
  split <- unlist(strsplit(exsolutionInput, ","))
  if(any(as.numeric(split) > total)){
    mensagem <- paste0('A alternativa correta é um número maior que a quantidade de alternativas')
    return(c(FALSE,mensagem))
  }
  # preenche de 0
  exsolution <- rep(0,total)
  exsolution[as.numeric(split)] <- 1
  if(length(exsolution[exsolution == 1]) == 1 ){
    extype <- 'extype: schoice'
  } else { extype <- 'extype: mchoice'}
  # answerlist2Input OPTIONAL
  if(qualAnswerlist == 1){
    valores <- exsolution
    valores[valores == 0] <- '# Falso\n'
    valores[valores == 1] <- '# Verdadeiro\n'
    valores <- paste(valores , collapse ="")
    answerlist2Input <- valores
  }
  # EXSOLUTION FINAL
  exsolution <- paste(exsolution , collapse ="")
  exsolution <- paste0('exsolution: ',exsolution)
  answerlist1Input <- gsub("#", "*", answerlist1Input)
  answerlist2Input <- gsub("#", "*", answerlist2Input)
  exshuffleInput <- paste0('exshuffle: ',exshuffleInput)
  cat(code,codeInput,question,questionInput,
      answerlist,answerlist1Input,
      solution,solutionInput,answerlist,answerlist2Input,metaInformation,exnameInput,
      extype,exsolution,exshuffleInput,
      file = paste0('usuarios/',user,'/EXS/',nameInput,'.Rmd'),sep = "\n")
  
  mensagem <- paste0('Exercício ',nameInput,' criado com sucesso!')
  return(c(TRUE,mensagem))
}



# INPUT
# 
# codeInput
# questionInput
# answerlist1Input
# solutionInput
# answerlist2Input # escrever ou não
# exnameInput
# exsolutionInput # quais alternativas estão corretas?
# exshuffleInput # quantas alternativas mostrar 
# nameInput
# 
# PRONTO
# 
# code
# question
# answerlist
# solution
# answerlist
# metainfo
# exname
# extype
# exsolution
# exshuffle

