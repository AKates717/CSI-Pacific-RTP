library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(bsicons)
library(reticulate)
library(gt)
library(readxl)
library(openxlsx)
library(DT)
library(here)
library(bslib)
library(janitor)
library(shinyWidgets)
library(shinytoastr)

source("helpers.R")

#
# Themes & Formatting ----
#



custom_theme <- bslib::bs_theme(
  version = 5,
  preset = "bootstrap",
  spacer = "0.5rem",
  # bg = "#FFF",
  # fg = "#000000",
  primary = "#000000"
)



#
# Paths & constants ----
#
SOURCE_PATH <- here("sample_data", "acl-protocol-criteria-2025.xlsx")  # your uploaded workbook
SOURCE_SHEET <- "criteria_full"                     # sheet to read measures from

TARGET_PATH  <-here("sample_data", "outcome_data.xlsx")
TARGET_SHEET     <- "Phase_data"

COLS <- c("Phase","Outcome Measure","Date", "Timestamp","Side","Value","Units","Notes")

#
# Helpers ----
#
empty_df <- function() {
  data.frame(
    "Phase"           = numeric(),
    "Outcome Measure" = character(),
    "Date"            = as.Date(character()),
    "Timestamp"       = character(),
    "Side"            = character(),
    "Value"           = numeric(),
    "Units"           = character(),
    "Notes"           = character(),
    check.names = FALSE
  )
}

ensure_workbook <- function() {
  # Create the target workbook with both phase sheets if missing
  if (!file.exists(TARGET_PATH)) {
    wb <- createWorkbook()
    addWorksheet(wb, TARGET_SHEET)
    
    writeData(wb, TARGET_SHEET, empty_df(), colNames = TRUE)
    
    saveWorkbook(wb, TARGET_PATH, overwrite = TRUE)
  } else {
    # If exists, ensure both sheets exist with headers
    wb <- loadWorkbook(TARGET_PATH)
    existing_sheets <- names(wb)
    if (!(TARGET_SHEET %in% existing_sheets)) {
      addWorksheet(wb, TARGET_SHEET); writeData(wb, TARGET_SHEET, empty_df(), colNames = TRUE)
    }
    saveWorkbook(wb, TARGET_PATH, overwrite = TRUE)
  }
}

read_current_data <- function(sheet_name) {
    df <- openxlsx::read.xlsx(TARGET_PATH, sheet = sheet_name, check.names = FALSE)
  df
}

append_row <- function(sheet_name, row_df) {
  wb <- loadWorkbook(TARGET_PATH)
  existing <- tryCatch(readWorkbook(TARGET_PATH, sheet = sheet_name), error = function(e) NULL)
  n_existing <- if (is.null(existing)) 0 else nrow(existing)
  start_row <- n_existing + 2  # +1 for header, then first empty row
  writeData(wb, sheet = sheet_name, x = row_df, startRow = start_row, colNames = FALSE, keepNA = TRUE)
  saveWorkbook(wb, TARGET_PATH, overwrite = TRUE)
}

#---- Load measure lookup from criteria_full sheet (robust to header variants)
load_measure_lookup <- function() {
  if (!file.exists(SOURCE_PATH)) {
    warning("Source Excel not found: ", SOURCE_PATH)
    return(data.frame(measure = character(), units = character(), phase = character(), stringsAsFactors = FALSE))
  }
  df <- tryCatch(read_excel(SOURCE_PATH, sheet = SOURCE_SHEET, .name_repair = "minimal"),
                 error = function(e) NULL)
  if (is.null(df)) {
    warning("Could not read sheet '", SOURCE_SHEET, "' from ", SOURCE_PATH)
    return(data.frame(measure = character(), units = character(), phase = character(), stringsAsFactors = FALSE))
  }
  
  # Column picking (no renaming or sorting; keep sheet order)
  nm <- trimws(tolower(names(df)))
  col_phase   <- which(nm %in% c("phase","phase ","phase_number","phase_no"))
  col_measure <- which(nm %in% c("outcome measure","outcome_measure","measure","measure_name"))
  col_units   <- which(nm %in% c("units","unit","default units","default_units"))
  
  if (!length(col_measure)) {
    warning("No 'Outcome Measure' column found in criteria_full.")
    return(data.frame(measure = character(), units = character(), phase = character(), stringsAsFactors = FALSE))
  }
  
  m <- as.character(df[[col_measure[1]]])
  u <- if (length(col_units)) as.character(df[[col_units[1]]]) else rep("", length(m))
  p_raw <- if (length(col_phase)) df[[col_phase[1]]] else rep(NA, length(m))
  
  normalize_phase <- function(x) {
    if (is.numeric(x)) return(paste0("Phase ", as.integer(x)))
    x <- trimws(as.character(x))
    if (!nzchar(x)) return(NA_character_)
    # If any digits appear, use "Phase <first number>"
    d <- regmatches(x, regexpr("\\d+", x))
    if (length(d) && nzchar(d)) return(paste0("Phase ", d))
    # Otherwise, keep a clean label (e.g., "All")
    tolower_first <- tolower(x)
    if (tolower_first %in% c("all", "any")) return(tools::toTitleCase(tolower_first))
    tools::toTitleCase(x)
  }
  
  p <- vapply(p_raw, normalize_phase, character(1))
  
  out <- data.frame(
    measure = trimws(m),
    units   = ifelse(is.na(u), "", trimws(u)),
    phase   = p,
    stringsAsFactors = FALSE
  )
  
  # Drop empties, de-dup while preserving first appearance (sheet order)
  out <- out[nzchar(out$measure), , drop = FALSE]
  out <- out[!duplicated(out[c("measure","phase")]), , drop = FALSE]
  
  out
}


MEASURES <- load_measure_lookup()






measures_by_phase <- function(phase_label) {
  if (nrow(MEASURES) == 0) return(character())
  hits <- MEASURES$measure[
    !is.na(MEASURES$phase) &
      (MEASURES$phase == phase_label | tolower(MEASURES$phase) %in% c("all", "any"))
  ]
  hits[!duplicated(hits)]
}


units_for_measure <- function(measure) {
  if (nrow(MEASURES) == 0) return(NA_character_)
  u <- MEASURES$units[match(measure, MEASURES$measure)]
  if (length(u) == 0) NA_character_ else ifelse(is.na(u) | !nzchar(u), NA_character_, u)
}















## -----------------------------------------------------------------------
## Reticulate / Python env
## -----------------------------------------------------------------------

use_condaenv("tindeq", required = TRUE)

## -----------------------------------------------------------------------
## Python BLE streaming engine (Bleak in a background thread)
## Defines:
##   start_stream(), stop_stream(), get_samples(clear=TRUE),
##   is_streaming(), get_debug()
## -----------------------------------------------------------------------

py_run_string("
import asyncio, struct, threading
from bleak import BleakScanner, BleakClient

# Tindeq UUIDs
CTRL_UUID = '7e4e1703-1ea6-40c9-9dcc-13d34ffead57'
DATA_UUID = '7e4e1702-1ea6-40c9-9dcc-13d34ffead57'

# Opcodes
OP_TARE  = 0x64
OP_START = 0x65
OP_STOP  = 0x66

def tlv_cmd(opcode, value=b''):
    return bytes([opcode, len(value)]) + value

# --------------------------------------------------------------------
# Shared state (Python-side)
# --------------------------------------------------------------------

_stream = {
  'running': False,
  'thread': None,
  'loop': None,
  'client': None,
  'samples': [],      # list of (t_us, weight)
  'lock': threading.Lock(),
  'name_prefix': 'Progressor_3177',
  'auto_tare': True
}

# Debug counters to diagnose effective sampling rate
_debug = {
  'packets': 0,     # BLE notifications received
  'samples': 0,     # weight samples decoded
  'last_len': 0,    # last notification payload length
  'last_t_us': None
}

def _append_sample(t_us, w):
    with _stream['lock']:
        _stream['samples'].append((t_us, w))
        if len(_stream['samples']) > 5000:
            _stream['samples'] = _stream['samples'][-5000:]

    _debug['samples'] += 1
    _debug['last_t_us'] = t_us

async def _run_stream():
    # Scan for device (fast startup)
    devices = await BleakScanner.discover(timeout=2.0)

    target = None
    for d in devices:
        if (d.name or '').startswith(_stream['name_prefix']):
            target = d
            break

    if target is None:
        _stream['running'] = False
        return

    # IMPORTANT:
    # Parse *all* TLVs in a single notification payload
    def handle_notify(sender, data: bytearray):
        b = bytes(data)
        _debug['packets'] += 1
        _debug['last_len'] = len(b)

        i = 0
        while i + 2 <= len(b):
            code = b[i]
            ln   = b[i + 1]
            j = i + 2 + ln
            if j > len(b):
                break

            val = b[i + 2 : j]

            # Weight measurement TLV (0x01):
            # value may contain MULTIPLE samples back-to-back:
            # each sample = float32 weight + uint32 timestamp (8 bytes)
            if code == 0x01 and ln >= 8:
                n = (ln // 8) * 8
                for k in range(0, n, 8):
                    w = struct.unpack('<f', val[k:k+4])[0]
                    t = struct.unpack('<I', val[k+4:k+8])[0]
                    _append_sample(t, w)

            i = j
            
            
            
            

    async with BleakClient(target) as client:
        _stream['client'] = client
        await client.start_notify(DATA_UUID, handle_notify)

        if _stream['auto_tare']:
            await client.write_gatt_char(CTRL_UUID, tlv_cmd(OP_TARE), response=False)
            await asyncio.sleep(0.2)

        await client.write_gatt_char(CTRL_UUID, tlv_cmd(OP_START), response=False)

        while _stream['running']:
            await asyncio.sleep(0.05)

        await client.write_gatt_char(CTRL_UUID, tlv_cmd(OP_STOP), response=False)
        await asyncio.sleep(0.1)
        await client.stop_notify(DATA_UUID)

    _stream['client'] = None

def _thread_main():
    loop = asyncio.new_event_loop()
    _stream['loop'] = loop
    asyncio.set_event_loop(loop)
    try:
        loop.run_until_complete(_run_stream())
    finally:
        try:
            loop.stop()
        except:
            pass
        loop.close()
        _stream['loop'] = None
        _stream['running'] = False

def start_stream(name_prefix='Progressor_3177', auto_tare=True, reset_buffer=True):
    if _stream['running']:
        return True

    _stream['name_prefix'] = name_prefix
    _stream['auto_tare'] = bool(auto_tare)

    if reset_buffer:
        with _stream['lock']:
            _stream['samples'] = []

        _debug['packets'] = 0
        _debug['samples'] = 0
        _debug['last_len'] = 0
        _debug['last_t_us'] = None

    _stream['running'] = True
    th = threading.Thread(target=_thread_main, daemon=True)
    _stream['thread'] = th
    th.start()
    return True

def stop_stream():
    _stream['running'] = False
    return True

def get_samples(clear=True):
    with _stream['lock']:
        out = list(_stream['samples'])
        if clear:
            _stream['samples'] = []
    return out

def is_streaming():
    return bool(_stream['running'])

def get_debug():
    return dict(_debug)
")



#Psych ----

#UI for ACL-RSI
psychQuestionUI <- function(
    id,
    question_text,
    left_label,
    right_label
) {
  
  div(
    class = "psych-field",
    
    tags$div(
      style = "font-weight:600; margin-bottom:10px;",
      question_text
    ),
    
    tags$div(
      style = "
        display:flex;
        align-items:center;
        gap:12px;
        width:100%;
      ",
      
      # LEFT LABEL
      tags$div(
        left_label,
        style = "
          width:170px;
          min-width:170px;
          text-align:right;
          font-size:0.85rem;
          line-height:1.2;
        "
      ),
      
      # RADIO BUTTONS
      tags$div(
        style = "
          flex:1;
          display:flex;
          justify-content:center;
        ",
        
        radioButtons(
          inputId = id,
          label = NULL,
          choices = seq(0, 100, by = 10),
          selected = character(0),
          inline = TRUE
        )
      ),
      
      # RIGHT LABEL
      tags$div(
        right_label,
        style = "
          width:170px;
          min-width:170px;
          text-align:left;
          font-size:0.85rem;
          line-height:1.2;
        "
      )
    )
  )
}


#ACL-RSI dataframe
questions <- read_excel(SOURCE_PATH, sheet = "acl_rsi")

#UI for TSK-11 — renders the full table for all questions
tsk11TableUI <- function(questions_df) {
  scale_labels <- c("Strongly\ndisagree", "Somewhat\ndisagree", "Somewhat\nagree", "Strongly\nagree")

  tags$table(
    class = "tsk11-table",
    tags$thead(
      tags$tr(
        tags$th("", class = "tsk11-q-header"),
        lapply(scale_labels, function(lbl) {
          tags$th(HTML(gsub("\n", "<br>", lbl)), class = "tsk11-scale-header")
        })
      )
    ),
    tags$tbody(
      lapply(seq_len(nrow(questions_df)), function(i) {
        id  <- questions_df$id[i]
        qno <- i
        tags$tr(
          class = if (i %% 2 == 0) "tsk11-row tsk11-row-even" else "tsk11-row",
          tags$td(questions_df$question[i], class = "tsk11-question"),
          lapply(1:4, function(val) {
            input_id <- paste0(id, "_radio")
            tags$td(
              class = "tsk11-cell",
              tags$label(
                class = "tsk11-radio-label",
                tags$input(
                  type    = "radio",
                  name    = id,
                  value   = val,
                  class   = "tsk11-radio",
                  onclick = paste0("Shiny.setInputValue('", id, "', this.value)")
                ),
                as.character(val)
              )
            )
          })
        )
      })
    )
  )
}

#TSK-11 dataframe
tsk11_questions <- read_excel(SOURCE_PATH, sheet = "TSK-11")
