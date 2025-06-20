theme_vgp <- function(base_family = 'sans', base_size = 12) {
  theme_minimal(base_family = base_family, base_size = base_size) +
    theme(
      plot.title = element_text(
        face = 'bold',
        size = base_size * 1.5,
        margin = margin(b = 10),
        color = 'black'
      ),
      plot.subtitle = element_text(
        size = base_size * 1.2,
        margin = margin(b = 8),
        color = 'black'
      ),
      plot.caption = element_text(
        size = base_size * 0.8,
        hjust = 1,
        color = 'black',
        margin = margin(t = 10)
      ),
      axis.title = element_text(
        size = base_size * 1.1,
        face = 'bold',
        color = 'black'
      ),
      axis.text = element_text(
        size = base_size,
        color = 'black'
      ),
      panel.grid = element_line(color = 'gray90', linewidth = 0.4),
      plot.background = element_rect(fill = 'white', color = NA),
      panel.background = element_rect(fill = 'white', color = NA),
      strip.background = element_rect(fill = 'gray90', color = NA),
      strip.text = element_text(face = 'bold', color = 'black'),
      legend.position = 'bottom',
      legend.title = element_text(
        face = 'bold',
        size = base_size * 1.0,
        color = 'black'
      ),
      legend.text = element_text(
        size = base_size * 0.9,
        color = 'black'
      ),
      legend.spacing = unit(0.5, 'line'),
      legend.margin = margin(t = 5, b = 5)
    )
}

palette_vgp <- c(
  'branco' = '#ffffff',
  'neve' = '#f1f5f8',
  
  'preto' = '#000000',
  'cinza' = '#343a40',
  
  'azul-1' = '#1f77b4',
  'azul-2' = '#2b3a67',
  
  'verde-1' = '#2ca02c',
  'verde-2' = '#234c35',
  
  'vermelho-1' = '#d62728',
  'vermelho-2' = '#9e2a2f',
  
  'laranja-1' = '#ff7f0e',
  'laranja-2' = '#c78b46',
  
  'roxo-1' = '#9467bd',
  'roxo-2' = '#9b8ab3'
)
