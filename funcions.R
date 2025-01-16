setwd("C:/Users/guillem.castellano/OneDrive - ctfc.cat/100_UOC/Semestre_3/Visualització de dades/PAC_final")

#Paquets necessaris

library(ggplot2)

# Funció AED
analisis_exploratorio <- function(data, variable, tipo) {
  if (tipo == 1) {
    cat("Resum estadístic de la variable", variable, ":\n")
    print(summary(data[[variable]]))
    
    hist(data[[variable]], main = paste("Histograma de", variable), xlab = variable, col = "lightblue", border = "black")
    boxplot(data[[variable]], main = paste("Boxplot de", variable), ylab = variable, col = "lightgreen")
    
  } else if (tipo == 0) {
    cat("Taula ed freq. per a", variable, ":\n")
    freq_table <- table(data[[variable]])
    print(freq_table)
    
    
    freq_df <- as.data.frame(freq_table)
    colnames(freq_df) <- c("Nivell", "Frequencia")
    
    
    ggplot(freq_df, aes(x = Nivel, y = Frecuencia)) +
      geom_bar(stat = "identity", fill = "lightblue", color = "black") +
      geom_text(aes(label = Frecuencia), vjust = -0.5) +
      labs(
        title = paste("Diagrama de barres de", variable),
        x = variable,
        y = "Frequencia"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else {
    cat("Tipus de variable no vàlid. Ha de ser 0 o 1.\n")
  }
}


