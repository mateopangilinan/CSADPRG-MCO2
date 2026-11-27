# Last Names: Acosta, Cagigas, Pangilinan, Po
# Language: R
# Paradigm: Procedural programming
# Run: Rscript MP_8.R.r [--reports]
# Reports use the latest valid PHP date as their reference date.
# The earliest transaction is treated as the account opening date.

data_file <- "bank_transactions.csv"
header <- c("account_name", "account_number", "date", "time",
            "currency", "amount", "type", "current_balance")
annual_rate <- 0.05
rates <- c(1, 62, 0.40, 84, 72, 9)
currencies <- c("Philippine Peso (PHP)", "United States Dollar (USD)",
                "Japanese Yen (JPY)", "British Pound Sterling (GBP)",
                "Euro (EUR)", "Chinese Yuan Renminbi (CNY)")
transactions <- list()
accounts <- list()
highest_number <- 999999999
input <- file("stdin", open = "r")

# Round monetary results to centavos using half-up rounding.
round_money <- function(value) {
    sign(value) * floor(abs(value) * 100 + 0.5 + 1e-8) / 100
}

money <- function(value) {
    formatC(round_money(value), format = "f", digits = 2, big.mark = ",")
}

plain <- function(value) {
    sprintf("%.2f", round_money(value))
}

ask <- function(prompt) {
    cat(prompt)
    answer <- readLines(input, n = 1, warn = FALSE)
    if (length(answer) == 0) {
        cat("\nInput ended.\n")
        quit(status = 0)
    }
    trimws(answer)
}

# Repeat the prompt until a whole number falls within the permitted range.
read_integer <- function(prompt, minimum, maximum) {
    repeat {
        text <- ask(prompt)
        if (grepl("^[+-]?[0-9]+$", text)) {
            value <- suppressWarnings(as.numeric(text))
            if (is.finite(value) && value >= minimum && value <= maximum) return(value)
        }
        cat("Enter a whole number from", minimum, "to", maximum, ".\n")
    }
}

# Validate numeric syntax and amount limits before accepting financial input.
parse_amount <- function(text) {
    pattern <- "^[+-]?([0-9]+|[0-9]{1,3}(,[0-9]{3})+)(\\.[0-9]+)?$"
    if (!grepl(pattern, text)) stop("Invalid financial value.")
    value <- as.numeric(gsub(",", "", text, fixed = TRUE))
    if (!is.finite(value) || value < 0 || value >= 1e12) stop("Invalid financial value.")
    value
}

# Check the original amount against the minimum before rounding.
read_amount <- function(prompt, minimum) {
    repeat {
        invalid_amount <- function(error) NA_real_
        value <- tryCatch(parse_amount(ask(prompt)), error = invalid_amount)
        if (!is.na(value) && value >= minimum && round_money(value) >= minimum) {
            return(round_money(value))
        }
        cat("Enter a valid amount of at least PHP", money(minimum), ".\n")
    }
}

# Accept only Y or N for repeated menu operations.
yes_no <- function(prompt) {
    repeat {
        answer <- toupper(ask(prompt))
        if (answer == "Y") return(TRUE)
        if (answer == "N") return(FALSE)
        cat("Enter Y or N.\n")
    }
}

# Parse quoted fields without losing leading zeroes in account numbers.
parse_csv <- function(line) {
    chars <- strsplit(line, "", fixed = TRUE)[[1]]
    fields <- character()
    field <- ""
    quoted <- FALSE
    closed <- FALSE
    i <- 1
    while (i <= length(chars)) {
        ch <- chars[i]
        if (quoted) {
            if (ch == '"') {
                if (i < length(chars) && chars[i + 1] == '"') {
                    field <- paste0(field, '"')
                    i <- i + 1
                } else {
                    quoted <- FALSE
                    closed <- TRUE
                }
            } else field <- paste0(field, ch)
        } else if (ch == ",") {
            fields <- c(fields, trimws(field))
            field <- ""
            closed <- FALSE
        } else if (closed) {
            if (nzchar(trimws(ch))) stop("Text after quoted field.")
        } else if (ch == '"') {
            if (nzchar(field)) stop("Unexpected quote.")
            quoted <- TRUE
        } else field <- paste0(field, ch)
        i <- i + 1
    }
    if (quoted) stop("Unclosed quoted field.")
    c(fields, trimws(field))
}

# Quote output fields and escape embedded quotation marks.
csv_line <- function(fields) {
    paste(paste0('"', gsub('"', '""', fields, fixed = TRUE), '"'), collapse = ",")
}

# Validate the calendar date, time, transaction type, and financial fields.
parse_transaction <- function(fields, row) {
    date <- as.Date(fields[3], format = "%d/%m/%Y")
    if (is.na(date) || format(date, "%d/%m/%Y") != fields[3] ||
        !grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", fields[3])) stop("Invalid date.")
    if (!grepl("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}$", fields[4])) stop("Invalid time.")
    clock <- as.integer(strsplit(fields[4], ":", fixed = TRUE)[[1]])
    if (clock[1] > 23 || clock[2] > 59 || clock[3] > 59) stop("Invalid time.")
    amount <- round_money(parse_amount(fields[6]))
    balance <- round_money(parse_amount(fields[8]))
    type <- tolower(fields[7])
    if (amount <= 0 || !type %in% c("deposit", "withdrawal")) stop("Invalid amount or type.")
    list(name = fields[1], number = fields[2], date = date,
         seconds = clock[1] * 3600 + clock[2] * 60 + clock[3],
         amount = amount, balance = balance, type = type, row = row)
}

# Group chronological history by account number and retain the latest balance.
rebuild_accounts <- function() {
    grouped <- list()
    for (tx in transactions) {
        id <- tx$number
        if (is.null(grouped[[id]])) {
            grouped[[id]] <- list(name = tx$name, number = id, opened = tx$date,
                                   history = list())
        }
        account <- grouped[[id]]
        account$balance <- tx$balance
        account$date <- tx$date
        account$seconds <- tx$seconds
        account$history[[length(account$history) + 1]] <- tx
        grouped[[id]] <- account
    }
    accounts <<- grouped[sort(names(grouped))]
}

# Validate into a temporary dataset before replacing the active accounts.
load_data <- function() {
    load_error <- function(error) {
        cat("Could not load", data_file, "-", conditionMessage(error), "\n")
        FALSE
    }
    tryCatch({
        if (!file.exists(data_file) || file.info(data_file)$size == 0) {
            writeLines(paste(header, collapse = ","), data_file, useBytes = TRUE)
        }
        lines <- readLines(data_file, warn = FALSE, encoding = "UTF-8")
        if (!identical(parse_csv(sub("^\ufeff", "", lines[1])), header)) stop("Unexpected CSV header.")
        loaded <- list()
        maximum <- -1
        invalid <- 0
        excluded <- 0
        for (row in seq_along(lines)[-1]) {
            invalid_row <- function(error) {
                invalid <<- invalid + 1
                cat("Row", row, "-", conditionMessage(error), "\n")
            }
            tryCatch({
                fields <- parse_csv(lines[row])
                if (length(fields) != 8 || !grepl("^[0-9]{10}$", fields[2])) stop("Invalid columns or account number.")
                maximum <- max(maximum, as.numeric(fields[2]))
                if (any(!nzchar(fields))) stop("Missing value.")
                if (toupper(fields[5]) != "PHP") {
                    excluded <- excluded + 1
                } else loaded[[length(loaded) + 1]] <- parse_transaction(fields, row)
            }, error = invalid_row)
        }
        if (length(loaded)) {
            dates <- numeric(length(loaded))
            seconds <- numeric(length(loaded))
            for (i in seq_along(loaded)) {
                dates[i] <- as.numeric(loaded[[i]]$date)
                seconds[i] <- loaded[[i]]$seconds
            }
            loaded <- loaded[order(dates, seconds, seq_along(loaded))]
        }
        transactions <<- loaded
        highest_number <<- maximum
        if (maximum < 0) highest_number <<- 999999999
        # Group chronological history by account number and retain the latest balance.
        rebuild_accounts()
        cat(sprintf("Rows read: %d; valid PHP: %d; non-PHP excluded: %d; invalid rows: %d.\n",
                    length(lines) - 1, length(loaded), excluded, invalid))
        TRUE
    }, error = load_error)
}

# Find an exact account number or unique name and allow zero to cancel.
select_account <- function() {
    if (!length(accounts)) {
        cat("Please register an account first.\n")
        return(NULL)
    }
    repeat {
        id <- ask("Account Name or Number (0 to cancel): ")
        if (id == "0") return(NULL)
        if (id %in% names(accounts)) return(accounts[[id]])
        lookup <- id
        parts <- strsplit(id, ",", fixed = TRUE)[[1]]
        if (length(parts) == 2) lookup <- paste(trimws(parts[2]), trimws(parts[1]))
        matches <- character()
        for (account in accounts) {
            if (tolower(account$name) %in% tolower(c(id, lookup))) matches <- c(matches, account$number)
        }
        if (length(matches) == 1) return(accounts[[matches]])
        if (length(matches) > 1) cat("Several accounts have that name. Enter the account number.\n")
        else cat("Account not found.\n")
    }
}

# Display the same account details for deposits, withdrawals, and interest.
print_account <- function(account) {
    cat("Account Name:", account$name, "\nAccount Number:", account$number,
        "\nCurrent Balance:", money(account$balance), "\nCurrency: PHP\n")
}

# Save the CSV record before updating the in-memory account balance.
save_transaction <- function(name, number, amount, type, balance) {
    save_error <- function(error) {
        cat("Transaction could not be saved:", conditionMessage(error), "\n")
        FALSE
    }
    now <- Sys.time()
    date <- as.Date(format(now, "%Y-%m-%d"))
    clock <- as.integer(strsplit(format(now, "%H:%M:%S"), ":")[[1]])
    seconds <- clock[1] * 3600 + clock[2] * 60 + clock[3]
    account <- accounts[[number]]
    if (balance >= 1e12 || (!is.null(account) &&
        (date < account$date || (date == account$date && seconds < account$seconds)))) {
        cat("Invalid balance or an existing future transaction. Check the dataset or system date.\n")
        return(FALSE)
    }
    fields <- c(name, number, format(date, "%d/%m/%Y"), format(now, "%H:%M:%S"),
                "PHP", plain(amount), type, plain(balance))
    tryCatch({
        size <- file.info(data_file)$size
        if (is.na(size) || size == 0) stop("Reload the transaction file first.")
        con <- file(data_file, "r+b")
        tryCatch({
            seek(con, size - 1, origin = "start", rw = "read")
            last <- readBin(con, "raw", 1)
            seek(con, 0, origin = "end", rw = "write")
            separator <- ""
            if (!as.integer(last) %in% c(10, 13)) separator <- "\n"
            writeBin(charToRaw(enc2utf8(paste0(separator, csv_line(fields), "\n"))), con)
        }, finally = close(con))
        transactions[[length(transactions) + 1]] <<- parse_transaction(fields, length(transactions) + 2)
        highest_number <<- max(highest_number, as.numeric(number))
        # Group chronological history by account number and retain the latest balance.
        rebuild_accounts()
        TRUE
    }, error = save_error)
}

# Assign the next available account number and save its initial deposit.
register_account <- function() {
    repeat {
        cat("\nRegister Account\n")
        if (highest_number >= 9999999999) {
            cat("No more ten-digit account numbers are available.\n")
            return(invisible(NULL))
        }
        name <- ask("Account Name (First name Last name): ")
        while (!grepl(" ", name, fixed = TRUE)) name <- ask("Enter a first name and last name: ")
        amount <- read_amount("Initial Deposit: ", 5000)
        number <- sprintf("%010.0f", highest_number + 1)
        if (save_transaction(name, number, amount, "deposit", amount)) {
            cat("Account created successfully!\n")
            # Display the same account details for deposits, withdrawals, and interest.
            print_account(accounts[[number]])
        }
        if (!yes_no("Register another account? (Y/N): ")) break
    }
}

# Share deposit and withdrawal prompts while checking the resulting balance.
transact <- function(type) {
    title <- "Deposit Amount"
    again <- "Deposit another amount? (Y/N): "
    if (type == "withdrawal") {
        title <- "Withdraw Amount"
        again <- "Withdraw another amount? (Y/N): "
    }
    repeat {
        cat("\n", title, "\n", sep = "")
        account <- select_account()
        if (is.null(account)) return(invisible(NULL))
        # Display the same account details for deposits, withdrawals, and interest.
        print_account(account)
        amount <- read_amount(paste0(title, ": "), 0.01)
        balance <- account$balance + amount
        if (type == "withdrawal") balance <- account$balance - amount
        balance <- round_money(balance)
        if (balance < 0) cat("Insufficient balance.\n")
        else if (save_transaction(account$name, account$number, amount, type, balance)) {
            cat("Updated Balance:", money(balance), "\n")
        }
        if (!yes_no(again)) break
    }
}

print_currencies <- function() {
    for (i in seq_along(currencies)) cat(sprintf("[%d] %s\n", i, currencies[i]))
}

# Convert the source amount through PHP into the selected target currency.
currency_exchange <- function() {
    repeat {
        cat("\nForeign Currency Exchange\n")
        print_currencies()
        source <- read_integer("Source Currency: ", 1, 6)
        amount <- read_amount("Source Amount: ", 0.01)
        target <- read_integer("Exchange Currency: ", 1, 6)
        cat("Exchange Amount:", money(amount * rates[source] / rates[target]), "\n")
        if (!yes_no("Convert another currency (Y/N)? ")) break
    }
}

# Update a foreign exchange rate for this session while keeping PHP fixed at one.
record_rate <- function() {
    print_currencies()
    choice <- read_integer("Select Foreign Currency: ", 1, 6)
    if (choice == 1) cat("PHP is the base currency. Its rate is always 1.00.\n")
    else {
        rates[choice] <<- read_amount("Exchange Rate (PHP per unit): ", 0.01)
        cat("Exchange rate updated.\n")
    }
}

# Display daily compounded interest without saving projected deposits.
show_interest <- function() {
    account <- select_account()
    if (is.null(account)) return(invisible(NULL))
    # Display the same account details for deposits, withdrawals, and interest.
    print_account(account)
    cat("Interest Rate: 5% per annum\n")
    days <- read_integer("Total Number of Days: ", 1, 36500)
    balance <- account$balance
    cat("Day | Interest | Balance |\n")
    for (day in seq_len(days)) {
        interest <- round_money(balance * annual_rate / 365)
        balance <- round_money(balance + interest)
        cat(day, "|", money(interest), "|", money(balance), "|\n")
    }
}

# Write the CSV report and display its first two rows as a preview.
write_report <- function(filename, headers, rows) {
    con <- file(filename, "wb")
    tryCatch({
        writeBin(charToRaw(paste0(csv_line(headers), "\n")), con)
        for (row in rows) writeBin(charToRaw(enc2utf8(paste0(csv_line(row), "\n"))), con)
    }, finally = close(con))
    cat(paste(headers, collapse = " | "), "\n")
    for (i in seq_len(min(2, length(rows)))) cat(paste(rows[[i]], collapse = " | "), "\n")
    cat(length(rows), "rows exported to", filename, "\n\n")
}

# Generate all reports from one dataset and aggregate the summary totals.
generate_reports <- function() {
    report_error <- function(error) {
        cat("Could not finish writing reports:", conditionMessage(error), "\n")
        FALSE
    }
    tryCatch({
        date <- Sys.Date()
        if (length(transactions)) {
            date <- transactions[[1]]$date
            for (tx in transactions) date <- max(date, tx$date)
        }
        first_current <- as.Date(format(date, "%Y-%m-01"))
        last_month <- first_current - 1 # End of the previous complete month.
        first_month <- as.Date(format(last_month, "%Y-%m-01"))
        month <- format(last_month, "%Y-%m")
        days_in_month <- as.integer(first_current - first_month)
        cat("Generating reports as of", format(date, "%d/%m/%Y"), "\n")
        tiers <- list()
        averages <- numeric()
        interests <- list()
        interest_values <- numeric()
        daily <- list()
        monthly <- list()
        counts <- list()
        peaks <- list()
        recency <- list()
        deposits <- 0
        withdrawals <- 0
        balances <- 0
        total_interest <- 0
        for (account in accounts) {
            closing <- 0
            sum_cents <- 0
            index <- 1
            history <- account$history
            for (day in seq_len(days_in_month)) {
                current <- first_month + day - 1
                while (index <= length(history) && history[[index]]$date <= current) {
                    closing <- round(history[[index]]$balance * 100)
                    index <- index + 1
                }
                sum_cents <- sum_cents + closing # Include inactive days.
            }
            average <- sum_cents / (100 * days_in_month)
            if (average >= 100000) {
                tier <- "Silver"
                if (average >= 1000000) tier <- "Platinum"
                else if (average >= 500000) tier <- "Gold"
                tiers[[length(tiers) + 1]] <- c(account$name, as.character(as.integer(date - account$opened)),
                    money(account$balance), money(average), tier)
                averages <- c(averages, average)
            }
            balances <- balances + account$balance
            if (account$balance >= 10000) {
                interest <- floor(round(account$balance * 100) * 30 / (365 / annual_rate) + 0.5) / 100
                interests[[length(interests) + 1]] <- c(account$name, money(account$balance), money(interest))
                interest_values <- c(interest_values, interest)
                total_interest <- total_interest + interest
            }
            peak <- 0
            for (tx in history) {
                day_key <- paste(format(tx$date, "%Y-%m-%d"), account$number, sep = "|")
                month_key <- paste(format(tx$date, "%Y-%m"), account$number, sep = "|")
                for (key in c(day_key, month_key)) {
                    is_day <- key == day_key
                    activity <- monthly[[key]]
                    if (is_day) activity <- daily[[key]]
                    if (is.null(activity)) activity <- list(name = account$name, number = account$number,
                        date = tx$date, deposits = 0, withdrawals = 0, deposit_count = 0, withdrawal_count = 0)
                    if (tx$type == "deposit") {
                        activity$deposits <- activity$deposits + round(tx$amount * 100)
                        activity$deposit_count <- activity$deposit_count + 1
                    } else {
                        activity$withdrawals <- activity$withdrawals + round(tx$amount * 100)
                        activity$withdrawal_count <- activity$withdrawal_count + 1
                    }
                    if (is_day) daily[[key]] <- activity
                    else monthly[[key]] <- activity
                }
                if (tx$type == "deposit") deposits <- deposits + tx$amount
                else {
                    withdrawals <- withdrawals + tx$amount
                    peak <- max(peak, tx$amount)
                }
            }
            # Frequency distinguishes many small movements from one large transfer.
            activity <- monthly[[paste(month, account$number, sep = "|")]]
            if (is.null(activity)) activity <- list(deposit_count = 0, withdrawal_count = 0, deposits = 0, withdrawals = 0)
            counts[[length(counts) + 1]] <- c(account$name, account$number, activity$deposit_count,
                activity$withdrawal_count, money(activity$deposits / 100), money(activity$withdrawals / 100),
                money((activity$deposits - activity$withdrawals) / 100))
            # Peak withdrawals measure cash demand. Recency identifies inactivity.
            peaks[[length(peaks) + 1]] <- c(account$name, account$number, money(peak))
            recency[[length(recency) + 1]] <- c(account$name, account$number,
                format(account$date, "%d/%m/%Y"), as.character(as.integer(date - account$date)))
        }
        # Write the CSV report and display its first two rows as a preview.
        write_report("report1_banking_privilege_tiers.csv",
            c("AccountName", "AccountAge", "CurrentBalance", "AverageDailyBalance", "PrivilegeTier"),
            tiers[order(-averages, seq_along(averages))])
        # Write the CSV report and display its first two rows as a preview.
        write_report("report2_interest_expense_projection.csv",
            c("AccountName", "CurrentBalance", "ExpectedInterest"),
            interests[order(-interest_values, seq_along(interest_values))])
        daily <- daily[sort(names(daily))]
        rows <- list()
        dates <- numeric()
        risks <- logical()
        for (activity in daily) {
            velocity <- "N/A" # No deposit total means turnover is undefined.
            turnover <- 0
            if (activity$deposits > 0) {
                turnover <- activity$withdrawals * 100 / activity$deposits
                velocity <- money(turnover)
            }
            risk <- "Normal"
            flagged <- activity$withdrawals >= 50000000 || turnover >= 90
            if (flagged) risk <- "Suspicious"
            dates <- c(dates, as.numeric(activity$date))
            risks <- c(risks, flagged)
            rows[[length(rows) + 1]] <- c(activity$name, format(activity$date, "%d/%m/%Y"),
                money(activity$withdrawals / 100), activity$deposit_count,
                money(activity$deposits / 100), velocity, risk)
        }
        # Write the CSV report and display its first two rows as a preview.
        write_report("report3_suspicious_client_behavior.csv",
            c("AccountName", "Date", "TotalWithdrawalAmounts", "NumberOfDeposits",
              "TotalDepositAmounts", "TurnoverVelocity (%)", "RiskFlagLevel"),
            rows[order(dates, -as.integer(risks), seq_along(rows))])
        # Net flows reveal monthly funding trends.
        rows <- list()
        for (key in sort(names(monthly))) {
            activity <- monthly[[key]]
            rows[[length(rows) + 1]] <- c(activity$name, activity$number, substr(key, 1, 7),
                money(activity$deposits / 100), money(activity$withdrawals / 100),
                money((activity$deposits - activity$withdrawals) / 100))
        }
        # Write the CSV report and display its first two rows as a preview.
        write_report("report4_monthly_net_cash_flow.csv",
            c("AccountName", "AccountNumber", "Month", "TotalDeposits", "TotalWithdrawals", "NetCashFlow"), rows)
        # Write the CSV report and display its first two rows as a preview.
        write_report("report4_monthly_account_activity.csv",
            c("AccountName", "AccountNumber", "DepositCount", "WithdrawalCount", "TotalDeposits", "TotalWithdrawals", "NetCashFlow"), counts)
        # Write the CSV report and display its first two rows as a preview.
        write_report("report4_largest_withdrawal.csv", c("AccountName", "AccountNumber", "LargestWithdrawal"), peaks)
        # Write the CSV report and display its first two rows as a preview.
        write_report("report4_account_recency.csv",
            c("AccountName", "AccountNumber", "LastTransactionDate", "DaysSinceLastTransaction"), recency)
        json <- sprintf(paste0("{\n  \"total_accounts\": %d,\n  \"total_deposits\": %s,\n",
            "  \"total_withdrawals\": %s,\n  \"total_balance\": %s,\n  \"total_interest_expense\": %s\n}\n"),
            length(accounts), plain(deposits), plain(withdrawals), plain(balances), plain(total_interest))
        con <- file("summary.json", "wb")
        tryCatch(writeBin(charToRaw(json), con), finally = close(con))
        cat("Summary State (summary.json):\n", json, sep = "")
        TRUE
    }, error = report_error)
}

# Allow repeated reloads and exports until the user selects Back.
report_menu <- function() {
    repeat {
        cat("\nGenerate Reports\n[1] Load the file\n[2] Generate Reports\n[0] Back\n")
        choice <- read_integer("Enter choice: ", 0, 2)
        if (choice == 0) break
        if (choice == 1) load_data()
        else generate_reports()
    }
}

# Load saved accounts and choose batch reports or the interactive menu.
main <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) > 1 || (length(args) == 1 && args[1] != "--reports")) {
        cat("Usage: Rscript MP_8.R.r [--reports]\n")
        quit(status = 1)
    }
    if (!load_data()) quit(status = 1)
    if (length(args) == 1) {
        if (!generate_reports()) quit(status = 1)
        return(invisible(NULL))
    }
    repeat {
        cat("\nMain Menu\nSelect Transaction:\n[1] Register Account\n[2] Deposit Amount\n",
            "[3] Withdraw Amount\n[4] Currency Exchange\n[5] Record Exchange Rates\n",
            "[6] Show Interest Amount\n[7] Generate Reports\n[0] Exit\n", sep = "")
        choice <- read_integer("Choice: ", 0, 7)
        if (choice == 0) break
        if (choice == 1) register_account()
        else if (choice == 2) transact("deposit")
        else if (choice == 3) transact("withdrawal")
        else if (choice == 4) currency_exchange()
        else if (choice == 5) record_rate()
        else if (choice == 6) show_interest()
        else if (choice == 7) report_menu()
        if (!yes_no("Back to the Main Menu (Y/N): ")) break
    }
    cat("Thank you for using the application!\n")
}

# Load saved accounts and choose batch reports or the interactive menu.
main()
