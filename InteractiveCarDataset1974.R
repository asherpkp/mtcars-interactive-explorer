library(tidyverse)
library(plotly)
library(htmlwidgets)
library(htmltools)

cars_df <- mtcars |>
  rownames_to_column("car") |>
  mutate(
    trans = ifelse(am == 1, "Manual", "Automatic"),
    cyl_label = paste(cyl, "Cylinders")
  )

# Editorial print-style palette (muted navy, ochre, terracotta)
cyl_colors <- c("4" = "#1d4ed8", "6" = "#d97706", "8" = "#b91c1c")

p <- ggplot(cars_df, aes(
  x = hp, 
  y = mpg, 
  color = factor(cyl), 
  size = wt,
  text = paste0(
    "<b>", car, "</b><br>",
    mpg, " MPG  •  ", hp, " HP<br>",
    round(wt * 1000), " lbs  •  ", trans
  )
)) +
  geom_point(alpha = 0.82) +
  scale_color_manual(
    values = cyl_colors,
    labels = c("4" = "4 cyl", "6" = "6 cyl", "8" = "8 cyl")
  ) +
  scale_size_continuous(range = c(3.5, 10), guide = "none") +
  labs(
    x = "Horsepower (hp)",
    y = "Miles per gallon (MPG)",
    color = NULL
  ) +
  theme_minimal(base_family = "Inter") +
  theme(
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    panel.grid.major = element_line(color = "#e5e7eb", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#4b5563", size = 11),
    axis.title = element_text(color = "#111827", size = 11, face = "bold")
  )

interactive_plot <- ggplotly(p, tooltip = "text", height = 490) |>
  layout(
    paper_bgcolor = "#ffffff",
    plot_bgcolor = "#ffffff",
    font = list(family = "Inter, sans-serif"),
    legend = list(
      orientation = "h",
      x = 0,
      y = 1.08,
      title = list(text = "<b>Cylinders:</b> "),
      font = list(family = "Inter, sans-serif", size = 12, color = "#374151")
    ),
    hoverlabel = list(
      bgcolor = "#111827",
      bordercolor = "#111827",
      font = list(family = "Inter, sans-serif", size = 12.5, color = "#ffffff")
    ),
    margin = list(l = 55, r = 20, t = 45, b = 50)
  ) |>
  config(displayModeBar = FALSE) |>
  onRender("
    function(el) {
      function loadCarDetails(carName, specsHtml, metaText) {
        var titleEl = document.getElementById('car-title');
        var metaEl = document.getElementById('car-meta');
        var specsEl = document.getElementById('car-specs');
        var imgEl = document.getElementById('car-photo');
        var descEl = document.getElementById('car-desc');
        
        titleEl.innerText = carName;
        metaEl.innerText = metaText;
        specsEl.innerHTML = specsHtml;
        descEl.innerText = 'Loading archive details...';
        
        // Search specifically for 1974 model to avoid modern car photos
        fetch('https://en.wikipedia.org/w/api.php?action=query&generator=search&gsrsearch=' + encodeURIComponent('1974 ' + carName) + '&gsrlimit=1&prop=pageimages|extracts&exintro=1&explaintext=1&exsentences=2&pithumbsize=600&format=json&origin=*')
          .then(res => res.json())
          .then(data => {
            if (!data.query || !data.query.pages) {
              imgEl.style.display = 'none';
              descEl.innerText = '1974 Motor Trend road test vehicle.';
              return;
            }
            var pages = data.query.pages;
            var firstPage = pages[Object.keys(pages)[0]];
            if (firstPage && firstPage.thumbnail) {
              imgEl.src = firstPage.thumbnail.source;
              imgEl.style.display = 'block';
            } else {
              imgEl.style.display = 'none';
            }
            descEl.innerText = (firstPage && firstPage.extract) ? firstPage.extract : '1974 Motor Trend road test vehicle.';
          })
          .catch(() => {
            descEl.innerText = '1974 Motor Trend road test vehicle.';
          });
      }

      // Default view: Toyota Corolla (1974)
      loadCarDetails(
        'Toyota Corolla',
        '<div class=\"stat\"><b>33.9</b><span>MPG</span></div>' +
        '<div class=\"stat\"><b>65</b><span>HP</span></div>' +
        '<div class=\"stat\"><b>1,835</b><span>LBS</span></div>',
        '4 Cylinders  •  Manual Transmission'
      );

      el.on('plotly_click', function(d) {
        var raw = d.points[0].text;
        var nameMatch = raw.match(/<b>(.*?)<\\/b>/);
        var mpgMatch = raw.match(/([0-9.]+) MPG/);
        var hpMatch = raw.match(/([0-9.]+) HP/);
        var wtMatch = raw.match(/([0-9]+) lbs/);
        var transMatch = raw.match(/lbs  •  (Manual|Automatic)/);
        if (!nameMatch) return;
        
        var specsHtml = 
          '<div class=\"stat\"><b>' + (mpgMatch ? mpgMatch[1] : '-') + '</b><span>MPG</span></div>' +
          '<div class=\"stat\"><b>' + (hpMatch ? hpMatch[1] : '-') + '</b><span>HP</span></div>' +
          '<div class=\"stat\"><b>' + (wtMatch ? Number(wtMatch[1]).toLocaleString() : '-') + '</b><span>LBS</span></div>';
          
        loadCarDetails(nameMatch[1], specsHtml, (transMatch ? transMatch[1] + ' Transmission' : '1974 Road Test'));
      });
    }
  ")

clean_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=Plus+Jakarta+Sans:wght@600;700&display=swap');

  * { box-sizing: border-box; margin: 0; padding: 0; }
  body {
    background-color: #f8f7f4;
    color: #111827;
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
    padding: 40px 24px;
    line-height: 1.5;
  }
  .wrap { max-width: 1180px; margin: 0 auto; }
  header {
    border-bottom: 2px solid #111827;
    padding-bottom: 16px;
    margin-bottom: 28px;
  }
  header h1 {
    font-family: 'Plus Jakarta Sans', sans-serif;
    font-size: 1.9rem;
    font-weight: 700;
    letter-spacing: -0.02em;
    color: #111827;
  }
  header p {
    color: #4b5563;
    font-size: 0.95rem;
    margin-top: 4px;
  }
  .grid {
    display: flex;
    flex-wrap: wrap;
    gap: 24px;
    align-items: flex-start;
  }
  .panel {
    background: #ffffff;
    border: 1px solid #d1d5db;
    border-radius: 6px;
  }
  .chart-panel {
    flex: 1 1 660px;
    padding: 18px 18px 10px 18px;
  }
  .detail-panel {
    flex: 1 1 330px;
    padding: 22px;
  }
  .eyebrow {
    font-size: 0.72rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    color: #6b7280;
    margin-bottom: 4px;
  }
  #car-title {
    font-family: 'Plus Jakarta Sans', sans-serif;
    font-size: 1.5rem;
    font-weight: 700;
    letter-spacing: -0.015em;
    color: #111827;
  }
  #car-meta {
    font-size: 0.85rem;
    color: #6b7280;
    margin-bottom: 14px;
  }
  .img-box {
    width: 100%;
    height: 205px;
    background: #f3f4f6;
    border: 1px solid #e5e7eb;
    border-radius: 4px;
    overflow: hidden;
    margin-bottom: 16px;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  #car-photo {
    width: 100%;
    height: 100%;
    object-fit: cover;
    display: none;
  }
  .stats {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    border-top: 1px solid #e5e7eb;
    border-bottom: 1px solid #e5e7eb;
    padding: 12px 0;
    margin-bottom: 14px;
    text-align: center;
  }
  .stat + .stat { border-left: 1px solid #e5e7eb; }
  .stat b {
    display: block;
    font-family: 'Plus Jakarta Sans', sans-serif;
    font-size: 1.2rem;
    font-weight: 700;
    color: #111827;
  }
  .stat span {
    font-size: 0.72rem;
    color: #6b7280;
    letter-spacing: 0.05em;
  }
  #car-desc {
    font-size: 0.88rem;
    color: #374151;
    line-height: 1.55;
  }
  footer {
    margin-top: 18px;
    font-size: 0.8rem;
    color: #6b7280;
  }
"

app_page <- tagList(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Motor Trend 1974: Horsepower vs. Fuel Efficiency"),
    tags$style(HTML(clean_css))
  ),
  div(class = "wrap",
      tags$header(
        tags$h1("Horsepower vs. Fuel Efficiency"),
        tags$p("1974 Motor Trend US road tests (n = 32). Dot size scales with vehicle weight; click any point to inspect.")
      ),
      div(class = "grid",
          div(class = "panel chart-panel", interactive_plot),
          div(class = "panel detail-panel",
              div(class = "eyebrow", "Vehicle Profile"),
              tags$h2(id = "car-title", "Toyota Corolla"),
              div(id = "car-meta", "4 Cylinders  •  Manual Transmission"),
              div(class = "img-box", tags$img(id = "car-photo", src = "")),
              div(id = "car-specs", class = "stats"),
              tags$p(id = "car-desc", "")
          )
      ),
      tags$footer("Source: Henderson & Velleman (1981), Building Multiple Regression Models Interactively, Biometrics.")
  )
)

save_html(app_page, file = "index.html")
browseURL("index.html")