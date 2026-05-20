build_selected_info <- function(
    prompt_text,
    selected_burn_id,
    cache_data
) {
  clicked_id <- selected_burn_id
  
  if (base::is.null(clicked_id)) {
    return(
      htmltools::div(
        class = "sa-empty-state sa-fade-in",
        htmltools::div(
          class = "sa-empty-state-text",
          prompt_text
        )
      )
    )
  }
  
  forecast_df <- cache_data$forecast_df
  sfog_tables <- cache_data$sfog_tables
  
  idx <- base::which(forecast_df$spot_id == clicked_id)
  
  if (base::length(idx) == 0) return(NULL)
  
  idx <- idx[1]
  selected_row <- forecast_df[idx, , drop = FALSE]
  
  spot_url <- selected_row$nws_spot_url[1]
  project <- selected_row$project_name[1]
  sfog_df <- sfog_tables[[idx]]
  
  issued_display <- if (
    "issuance_display" %in% names(selected_row) &&
    !base::is.na(selected_row$issuance_display[1])
  ) {
    selected_row$issuance_display[1]
  } else {
    format_issued_datetime(selected_row$issuanceTime[1])
  }
  
  if (base::is.null(sfog_df)) {
    return(
      htmltools::HTML(base::paste0(
        'Unable to screen. Please consult your <a href="',
        spot_url,
        '" target="_blank">spot forecast</a>.'
      ))
    )
  }
  
  
  sfog_status <- sfog_df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      critical_count = base::sum(
        dplyr::c_across(
          c(sky_screen, temp_screen, rh_screen, wind_screen)
        ) %in% c("critical", "watch_out")
      )
    ) |>
    dplyr::ungroup()
  
  total_max <- base::max(sfog_status$critical_count, na.rm = TRUE)
  
  if (total_max == 4) {
    
    sfog_box <- htmltools::div(
      style = "border:4px solid red; background-color:#FFDADA; color:black; padding:12px; font-size:15px; margin:10px;",
      
      htmltools::div(
        style = "font-weight:bold; font-size:18px; margin-bottom:6px;",
        "PB Piedmont Required"
      ),
      
      htmltools::HTML(
        'Superfog criteria have been met. Please run a <a href="https://piedmont.dri.edu/" target="_blank">PB Piedmont model</a>.'
      )
    )
    
  } else if (total_max == 3) {
    
    sfog_box <- htmltools::div(
      style = "border:4px solid orange; background-color:#FFE8CC; color:black; padding:12px; font-size:15px; margin:10px;",
      
      htmltools::div(
        style = "font-weight:bold; font-size:18px; margin-bottom:6px;",
        "PB Piedmont Recommended"
      ),
      
      htmltools::HTML(
        'Most superfog criteria have been met. Running a <a href="https://piedmont.dri.edu/" target="_blank">PB Piedmont model</a> is recommended.'
      )
    )
    
  } else {
    
    sfog_box <- htmltools::div(
      style = "border:4px solid #777777; background-color:#D9D9D9; color:black; padding:12px; font-size:15px; margin:10px;",
      
      htmltools::div(
        style = "font-weight:bold; font-size:18px; margin-bottom:6px;",
        "PB Piedmont Not Required"
      ),
      
      "Superfog criteria have not been met."
    )
  }
  
  kbl_table <- sfog_df |>
    dplyr::mutate(
      
      SKY = kableExtra::cell_spec(
        SKY,
        format = "html",
        extra_css = base::sapply(sky_screen, function(v) {
          
          css <- if (v == "critical") {
            "background-color:#CA0020;color:white;font-weight:bold;text-align:center;"
          } else if (v == "watch_out") {
            "background-color:#FFDA00;color:black;font-weight:bold;text-align:center;"
          } else {
            "background-color:#D9D9D9;color:black;font-weight:bold;text-align:center;"
          }
          
          base::paste0(
            "display:block;width:100%;height:100%;",
            css
          )
        })
      ),
      
      TEMP = kableExtra::cell_spec(
        TEMP,
        format = "html",
        extra_css = base::sapply(temp_screen, function(v) {
          
          css <- if (v == "critical") {
            "background-color:#CA0020;color:white;font-weight:bold;text-align:center;"
          } else if (v == "watch_out") {
            "background-color:#FFDA00;color:black;font-weight:bold;text-align:center;"
          } else {
            "background-color:#D9D9D9;color:black;font-weight:bold;text-align:center;"
          }
          
          base::paste0(
            "display:block;width:100%;height:100%;",
            css
          )
        })
      ),
      
      RH = kableExtra::cell_spec(
        RH,
        format = "html",
        extra_css = base::sapply(rh_screen, function(v) {
          
          css <- if (v == "critical") {
            "background-color:#CA0020;color:white;font-weight:bold;text-align:center;"
          } else if (v == "watch_out") {
            "background-color:#FFDA00;color:black;font-weight:bold;text-align:center;"
          } else {
            "background-color:#D9D9D9;color:black;font-weight:bold;text-align:center;"
          }
          
          base::paste0(
            "display:block;width:100%;height:100%;",
            css
          )
        })
      ),
      
      WIND = kableExtra::cell_spec(
        WIND,
        format = "html",
        extra_css = base::sapply(wind_screen, function(v) {
          
          css <- if (v == "critical") {
            "background-color:#CA0020;color:white;font-weight:bold;text-align:center;"
          } else if (v == "watch_out") {
            "background-color:#FFDA00;color:black;font-weight:bold;text-align:center;"
          } else {
            "background-color:#D9D9D9;color:black;font-weight:bold;text-align:center;"
          }
          
          base::paste0(
            "display:block;width:100%;height:100%;",
            css
          )
        })
      )
    ) |>
    dplyr::select(
      DATETIME,
      TEMP,
      RH,
      WIND,
      SKY
    ) |>
    kableExtra::kbl(
      escape = FALSE,
      align = "c",
      col.names = c(
        "DATE-TIME",
        "Temperature<br>(°F)",
        "Relative<br>Humidity (%)",
        "Wind<br>Speed (mph)",
        "Cloud<br>Cover (%)"
      )
    ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      font_size = 16
    )
  
  htmltools::tagList(
    
    htmltools::h3(
      style = "font-weight:bold; font-size:24px;",
      project
    ),
    
    htmltools::div(
      style = "margin-bottom:6px; font-size:16px;",
      htmltools::a(
        "Full Spot Weather Forecast",
        href = spot_url,
        target = "_blank"
      )
    ),
    
    htmltools::div(
      style = "margin-bottom:10px; font-size:16px; color:#555;",
      base::paste0("Date Issued: ", issued_display)
    ),
    
    sfog_box,
    
    htmltools::HTML(base::as.character(kbl_table)),
    
    htmltools::div(
      style = "text-align:center; font-size:18px; margin-top:8px;",
      
      htmltools::span(
        style = "background-color:#CA0020;color:white;padding:8px 12px;margin-right:6px;font-weight:bold;",
        "Critical"
      ),
      
      htmltools::span(
        style = "background-color:#FFDA00;color:black;padding:8px 12px;margin-right:6px;font-weight:bold;",
        "Watch Out"
      ),
      
      htmltools::span(
        style = "background-color:#D9D9D9;color:black;padding:8px 12px;margin-right:6px;font-weight:bold;",
        "Minimal Concern"
      )
    )
  )
}