/*
Last Names: Acosta, Cagigas, Pangilinan, Po
Language: Kotlin
Paradigm(s): Object-Oriented Programming (OOP) and Procedural Programming
*/

import java.io.File
import java.io.EOFException
import java.io.IOException
import java.math.BigDecimal
import java.math.RoundingMode
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.YearMonth
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import java.time.format.ResolverStyle
import java.time.temporal.ChronoUnit
import java.util.Locale
import java.util.TreeMap

// Shared settings keep filenames, dates, and financial calculations consistent.
const val DATA_FILE = "bank_transactions.csv"
const val CSV_HEADER = "account_name,account_number,date,time,currency,amount,type,current_balance"
const val ANNUAL_INTEREST_RATE = 0.05
val dateFormat = DateTimeFormatter.ofPattern("dd/MM/uuuu", Locale.US)
    .withResolverStyle(ResolverStyle.STRICT)
val timeFormat = DateTimeFormatter.ofPattern("H:mm:ss", Locale.US)
    .withResolverStyle(ResolverStyle.STRICT)
val moneyPattern = Regex("""[+-]?([0-9]+|[0-9]{1,3}(,[0-9]{3})+)(\.[0-9]+)?""")
val inputTimePattern = Regex("""[0-9]{1,2}:[0-9]{2}:[0-9]{2}""")
val outputTimeFormat = DateTimeFormatter.ofPattern("HH:mm:ss", Locale.US)

// Each currency rate is the PHP value of one unit of that currency.
val currencyNames = arrayOf(
    "Philippine Peso (PHP)", "United States Dollar (USD)",
    "Japanese Yen (JPY)", "British Pound Sterling (GBP)",
    "Euro (EUR)", "Chinese Yuan Renminbi (CNY)"
)
val rates = doubleArrayOf(1.0, 62.0, 0.40, 84.0, 72.0, 9.0)

// An account owns its transactions and its latest recorded PHP balance.
class Account(val name: String, val number: String, val opened: LocalDate) {
    var balance = 0.0
    val transactions = ArrayList<Transaction>()
}

// A transaction stores the information from one valid PHP row in the CSV.
class Transaction(
    val accountName: String,
    val accountNumber: String,
    val timestamp: LocalDateTime,
    val amount: Double,
    val type: String,
    val balance: Double,
    val rowNumber: Int
) : Comparable<Transaction> {
    override fun compareTo(other: Transaction): Int = timestamp.compareTo(other.timestamp)
}

// Report 1 keeps each account together with its calculated monthly average.
class PrivilegeRow(val account: Account, val average: Double) : Comparable<PrivilegeRow> {
    override fun compareTo(other: PrivilegeRow): Int = other.average.compareTo(average)
}

// Report 2 keeps each account together with its projected interest.
class InterestRow(val account: Account, val interest: Double) : Comparable<InterestRow> {
    override fun compareTo(other: InterestRow): Int = other.interest.compareTo(interest)
}

// Report 3 collects one account's activity for one calendar day.
class DailyActivity(val account: Account, val date: LocalDate) : Comparable<DailyActivity> {
    override fun compareTo(other: DailyActivity): Int {
        if (date != other.date) return date.compareTo(other.date)
        return other.isSuspicious().compareTo(isSuspicious())
    }

    var withdrawals = 0.0
    var depositCount = 0
    var deposits = 0.0

    // Division is only allowed when deposits are greater than zero.
    // The report displays N/A separately for withdrawal-only days.
    fun turnover(): Double {
        if (deposits > 0.0) {
            return Math.round(withdrawals * 100) * 100.0 / Math.round(deposits * 100)
        } else {
            return 0.0
        }
    }

    fun isSuspicious(): Boolean {
        return withdrawals >= 500000.0 || (deposits > 0 && turnover() >= 90.0)
    }
}

// Report 4 groups deposits and withdrawals by account and calendar month.
class MonthlyActivity(val account: Account, val month: YearMonth) {
    var depositCount = 0
    var withdrawalCount = 0
    var deposits = 0.0
    var withdrawals = 0.0
}

// Look up accounts by number. TreeMap keeps the account numbers in ascending order.
val accounts = TreeMap<String, Account>()
val transactions = ArrayList<Transaction>()
var highestAccountNumber = 999999999L
var nextRowNumber = 2
var dataLoaded = false

// Read one answer. EOF means the input has ended, so the main menu can exit.
fun readInput(prompt: String): String {
    print(prompt)
    val input = readlnOrNull()
    if (input == null) {
        throw EOFException()
    } else {
        return input.trim()
    }
}

// BigDecimal rounds exact half-cent values up, such as 1.005 becoming 1.01.
fun roundMoney(value: Double): Double {
    return BigDecimal.valueOf(value).setScale(2, RoundingMode.HALF_UP).toDouble()
}

// Reports use grouping commas, while saved transactions and JSON use plain numbers.
fun formatMoney(value: Double): String {
    return String.format(Locale.US, "%,.2f", value)
}

fun formatPlainMoney(value: Double): String {
    return String.format(Locale.US, "%.2f", value)
}

// Invalid numbers are rejected without terminating the program.
fun readInteger(prompt: String, minimum: Int, maximum: Int): Int {
    while (true) {
        try {
            val value = readInput(prompt).toInt()
            if (value >= minimum && value <= maximum) {
                return value
            } else {
                println("Error: Enter a number from $minimum to $maximum.")
            }
        } catch (error: NumberFormatException) {
            println("Error: Enter a whole number.")
        }
    }
}

// Keep asking until the user enters an amount that meets the minimum.
fun readAmount(prompt: String, minimum: Double): Double {
    while (true) {
        try {
            val value = parseAmount(readInput(prompt))
            if (!value.isFinite() || value <= 0.0) {
                println("Error: Enter a positive, finite amount.")
            } else {
                val amount = roundMoney(value)
                if (value >= minimum && amount >= minimum) {
                    return amount
                } else {
                    println("Error: Amount must be at least PHP " + formatMoney(minimum) + ".")
                }
            }
        } catch (error: NumberFormatException) {
            println("Error: Enter a valid amount.")
        }
    }
}

// Accept Y or N regardless of capitalization.
fun askYesNo(prompt: String): Boolean {
    while (true) {
        val answer = readInput(prompt).uppercase(Locale.US)
        if (answer == "Y") {
            return true
        } else if (answer == "N") {
            return false
        } else {
            println("Error: Enter Y or N only.")
        }
    }
}

// CSV fields are quoted so commas and quotation marks in names remain valid.
fun csvLine(fields: List<String>): String {
    val line = StringBuilder()
    for (field in fields) {
        if (line.isNotEmpty()) {
            line.append(',')
        }
        line.append('"')
        line.append(field.replace("\"", "\"\""))
        line.append('"')
    }
    return line.toString()
}

// Parse one CSV record, including quoted commas and doubled quotation marks.
fun parseCsvLine(line: String): ArrayList<String> {
    val fields = ArrayList<String>()
    val field = StringBuilder()
    var quoted = false
    var closedQuote = false
    var index = 0
    while (index < line.length) {
        val character = line[index]
        if (quoted) {
            if (character == '"') {
                if (index + 1 < line.length && line[index + 1] == '"') {
                    field.append('"')
                    index++
                } else {
                    quoted = false
                    closedQuote = true
                }
            } else {
                field.append(character)
            }
        } else if (character == ',') {
            fields.add(field.toString().trim())
            field.setLength(0)
            closedQuote = false
        } else if (closedQuote) {
            if (!character.isWhitespace()) {
                throw IllegalArgumentException("Unexpected text after a quoted field.")
            }
        } else if (character == '"') {
            if (field.isNotEmpty()) {
                throw IllegalArgumentException("Quotation mark inside an unquoted field.")
            }
            quoted = true
        } else {
            field.append(character)
        }
        index++
    }
    if (quoted) {
        throw IllegalArgumentException("Unclosed quoted field; records must occupy one line.")
    }
    fields.add(field.toString().trim())
    return fields
}

// Account numbers are kept as strings, preserving their required ten digits.
fun validAccountNumber(number: String): Boolean {
    if (number.length != 10) {
        return false
    }
    for (character in number) {
        if (character < '0' || character > '9') {
            return false
        }
    }
    return true
}

// Convert one PHP row into named values and reject invalid dates, money, or types.
fun parseAmount(text: String): Double {
    if (!moneyPattern.matches(text)) {
        throw NumberFormatException("Invalid financial value.")
    }
    val value = text.replace(",", "").toDouble()
    if (!value.isFinite() || value >= 1.0e12) throw NumberFormatException("Amount is too large.")
    return value
}

// Validate the calendar date, time, transaction type, and financial fields.
fun parseTransaction(fields: List<String>, rowNumber: Int): Transaction {
    val accountName = fields[0]
    val accountNumber = fields[1]
    val date = LocalDate.parse(fields[2], dateFormat)
    if (date.year !in 1..9999) throw IllegalArgumentException("Invalid year.")
    if (!inputTimePattern.matches(fields[3])) throw IllegalArgumentException("Invalid time.")
    val time = LocalTime.parse(fields[3], timeFormat)
    val amount = parseAmount(fields[5])
    val type = fields[6].lowercase(Locale.US)
    val balance = parseAmount(fields[7])

    if (!amount.isFinite() || !balance.isFinite() || amount <= 0.0 || balance < 0.0) {
        throw IllegalArgumentException("Invalid amount or balance.")
    }
    if (type != "deposit" && type != "withdrawal") {
        throw IllegalArgumentException("Type must be deposit or withdrawal.")
    }
    if (roundMoney(amount) <= 0.0) {
        throw IllegalArgumentException("Amount rounds to zero.")
    }
    return Transaction(accountName, accountNumber, date.atTime(time),
        // Round monetary results to centavos using half-up rounding.
        roundMoney(amount), type, roundMoney(balance), rowNumber)
}

// Add a transaction to its account. Return 1 if a history warning was logged.
fun addLoadedTransaction(
    transaction: Transaction,
    loadedAccounts: TreeMap<String, Account>,
    log: StringBuilder
): Int {
    var warningCount = 0
    var account = loadedAccounts[transaction.accountNumber]
    if (account == null) {
        account = Account(transaction.accountName, transaction.accountNumber, transaction.timestamp.toLocalDate())
        loadedAccounts[account.number] = account
    } else {
        var expectedBalance = account.balance + transaction.amount
        if (transaction.type == "withdrawal") {
            expectedBalance = account.balance - transaction.amount
        }
        if (!expectedBalance.isFinite() || roundMoney(expectedBalance) != transaction.balance ||
            account.name != transaction.accountName) {
            warningCount = 1
            log.append("Row ${transaction.rowNumber}: Name or balance differs from prior history; ")
            log.append("recorded balance retained.\n")
        }
    }
    account.balance = transaction.balance
    account.transactions.add(transaction)
    return warningCount
}

// Read the CSV into temporary lists, then replace the active data after a successful load.
fun loadData(): Boolean {
    val file = File(DATA_FILE)
    val loadedTransactions = ArrayList<Transaction>()
    val log = StringBuilder()
    var highestNumber = -1L
    var rowCount = 0
    var excludedCount = 0
    var errorCount = 0
    var warningCount = 0
    try {
        if (!file.exists() || file.length() == 0L) {
            file.writeText(CSV_HEADER + "\n", Charsets.UTF_8)
        }
        // A 10,000-row dataset fits in memory; readLines also closes the file for us.
        val lines = file.readLines(Charsets.UTF_8)
        if (lines.isEmpty() || parseCsvLine(lines[0].removePrefix("\uFEFF")) != parseCsvLine(CSV_HEADER)) {
            throw IllegalArgumentException("The CSV header does not match the required eight columns.")
        }
        var index = 1
        while (index < lines.size) {
            rowCount++
            try {
                val fields = parseCsvLine(lines[index])
                if (fields.size != 8) {
                    throw IllegalArgumentException("Expected eight fields.")
                }

                // Reserve numbers from excluded and invalid rows as well as valid PHP rows.
                if (validAccountNumber(fields[1])) {
                    val number = fields[1].toLong()
                    if (number > highestNumber) {
                        highestNumber = number
                    }
                }
                for (field in fields) {
                    if (field.isBlank()) {
                        throw IllegalArgumentException("Missing field value.")
                    }
                }
                if (!validAccountNumber(fields[1])) {
                    throw IllegalArgumentException("Account number must contain ten digits.")
                }

                // Only PHP transactions are used in the accounts and reports.
                if (fields[4].equals("PHP", true)) {
                    loadedTransactions.add(parseTransaction(fields, index + 1))
                } else {
                    excludedCount++
                }
            } catch (error: DateTimeParseException) {
                errorCount++
                log.append("Row ${index + 1}: Invalid date or time.\n")
            } catch (error: IllegalArgumentException) {
                errorCount++
                log.append("Row ${index + 1}: ${error.message}\n")
            }
            index++
        }

        // Sort before rebuilding accounts so the latest balance is chosen by timestamp.
        loadedTransactions.sort()
        val loadedAccounts = TreeMap<String, Account>()
        for (transaction in loadedTransactions) {
            warningCount += addLoadedTransaction(transaction, loadedAccounts, log)
        }
        transactions.clear()
        transactions.addAll(loadedTransactions)
        accounts.clear()
        accounts.putAll(loadedAccounts)
        if (highestNumber < 0L) {
            highestNumber = 999999999L
        }
        highestAccountNumber = highestNumber
        nextRowNumber = rowCount + 2
        dataLoaded = true
    } catch (error: IOException) {
        println("Error: Could not read $DATA_FILE. " + error.message)
        return false
    } catch (error: IllegalArgumentException) {
        println("Error: Could not load $DATA_FILE. " + error.message)
        return false
    }

    // The log records real row counts instead of assuming the sample has 10,000 rows.
    val summary = "Rows read: $rowCount; valid PHP: ${transactions.size}; non-PHP excluded: $excludedCount; " +
        "invalid rows: $errorCount; history warnings: $warningCount."
    println(summary)
    println("Accounts loaded: " + accounts.size)
    try {
        File("ingestion_errors.log").writeText(summary + "\n" + log, Charsets.UTF_8)
    } catch (error: IOException) {
        println("Warning: Could not write ingestion_errors.log. " + error.message)
    }
    return true
}

// An account number is unambiguous; an exact name also works if only one account matches.
fun selectAccount(): Account? {
    if (accounts.isEmpty()) {
        println("Error: Please register an account first.")
        return null
    }
    while (true) {
        val input = readInput("Account Name or Number (0 to cancel): ")
        if (input == "0") {
            return null
        }
        var lookup = input
        val comma = input.indexOf(',')
        if (comma > 0 && input.indexOf(',', comma + 1) < 0) {
            lookup = input.substring(comma + 1).trim() + " " + input.substring(0, comma).trim()
        }
        val numberedAccount = accounts[input]
        if (numberedAccount != null) {
            return numberedAccount
        }
        var match: Account? = null
        var matches = 0
        for (account in accounts.values) {
            if (account.name.equals(input, true) || account.name.equals(lookup, true)) {
                match = account
                matches++
            }
        }
        if (matches == 1) {
            return match
        } else if (matches > 1) {
            println("Several accounts have that name. Enter the account number.")
        } else {
            println("Error: Account not found.")
        }
    }
}

// Reuse the same account display for deposits, withdrawals, and interest.
fun printAccount(account: Account) {
    println("Account Name: " + account.name)
    println("Account Number: " + account.number)
    println("Current Balance: " + formatMoney(account.balance))
    println("Currency: PHP")
}

// Save first, then update memory; a failed write must not change the account balance.
fun saveTransaction(account: Account, amount: Double, type: String): Boolean {
    val now = LocalDateTime.now().withNano(0)
    if (account.transactions.isNotEmpty()) {
        val latest = account.transactions[account.transactions.size - 1]
        if (now.isBefore(latest.timestamp)) {
            println("Error: This account has a future transaction dated " +
                latest.timestamp.toLocalDate().format(dateFormat) + ". Check the dataset or system date.")
            return false
        }
    }
    var updatedBalance = account.balance + amount
    if (type == "withdrawal") {
        updatedBalance = account.balance - amount
    }
    if (!updatedBalance.isFinite() || updatedBalance < 0.0 || updatedBalance >= 1.0e12) {
        println("Error: The resulting balance is invalid.")
        return false
    }
    updatedBalance = roundMoney(updatedBalance)
    val fields = listOf(account.name, account.number, now.toLocalDate().format(dateFormat),
        now.toLocalTime().format(outputTimeFormat), "PHP", formatPlainMoney(amount), type, formatPlainMoney(updatedBalance))
    try {
        // Check the ending before appending so the new row stays on its own line.
        val file = File(DATA_FILE)
        val contents = file.readText(Charsets.UTF_8)
        if (contents.isBlank()) {
            throw IOException("The transaction file is empty. Reload the file first.")
        }
        var separator = ""
        if (!contents.endsWith("\n") && !contents.endsWith("\r")) {
            separator = "\n"
        }
        file.appendText(separator + csvLine(fields) + "\n", Charsets.UTF_8)
    } catch (error: IOException) {
        println("Error: Transaction could not be saved. " + error.message)
        return false
    }
    val transaction = Transaction(account.name, account.number, now, amount, type,
        updatedBalance, nextRowNumber)
    account.balance = updatedBalance
    account.transactions.add(transaction)
    transactions.add(transaction)
    nextRowNumber++
    return true
}

// A new account's initial deposit is its first saved transaction.
fun registerAccount() {
    var again = true
    while (again) {
        println("\nRegister Account")
        if (highestAccountNumber >= 9999999999L) {
            println("Error: No more ten-digit account numbers are available.")
            return
        }
        var name = readInput("Account Name (First name Last name): ")
        while (!name.contains(' ')) {
            println("Error: Enter a first name and last name.")
            name = readInput("Account Name: ")
        }
        val amount = readAmount("Initial Deposit: ", 5000.0)
        val number = (highestAccountNumber + 1).toString().padStart(10, '0')
        val account = Account(name, number, LocalDate.now())
        if (saveTransaction(account, amount, "deposit")) {
            accounts[number] = account
            highestAccountNumber++
            println("Account created successfully!")
            // Display the same account details for deposits, withdrawals, and interest.
            printAccount(account)
        }
        again = askYesNo("Register another account? (Y/N): ")
    }
}

// Deposits and withdrawals share account selection and validation.
fun transact(type: String) {
    var title = "Deposit Amount"
    var repeatPrompt = "Deposit another amount? (Y/N): "
    if (type == "withdrawal") {
        title = "Withdraw Amount"
        repeatPrompt = "Withdraw another amount? (Y/N): "
    }
    var again = true
    while (again) {
        println("\n" + title)
        val account = selectAccount()
        if (account == null) {
            return
        }
        // Display the same account details for deposits, withdrawals, and interest.
        printAccount(account)
        val amount = readAmount(title + ": ", 0.01)
        if (type == "withdrawal" && amount > account.balance) {
            println("Error: Insufficient balance.")
        } else if (saveTransaction(account, amount, type)) {
            println("Updated Balance: " + formatMoney(account.balance))
        }
        again = askYesNo(repeatPrompt)
    }
}

// Currency conversion remains a calculator and does not alter PHP account balances.
fun printCurrencies() {
    var index = 0
    while (index < currencyNames.size) {
        println("[${index + 1}] " + currencyNames[index])
        index++
    }
}

// Convert the source amount to PHP first, then divide by the target currency's rate.
fun convert(amount: Double, from: Int, to: Int): Double {
    return amount * rates[from - 1] / rates[to - 1]
}

// Let the user repeat conversions without changing an account's saved balance.
fun currencyExchange() {
    var again = true
    while (again) {
        println("\nForeign Currency Exchange")
        printCurrencies()
        val source = readInteger("Source Currency: ", 1, currencyNames.size)
        val amount = readAmount("Source Amount: ", 0.01)
        val target = readInteger("Exchange Currency: ", 1, currencyNames.size)
        val result = convert(amount, source, target)
        if (result.isFinite()) {
            println("Exchange Amount: " + formatMoney(result))
        } else {
            println("Error: Conversion result is too large.")
        }
        again = askYesNo("Convert another currency (Y/N)? ")
    }
}

// Change a foreign currency's rate for this session; PHP always stays at 1.00.
fun recordExchangeRate() {
    println("\nRecord Exchange Rate")
    printCurrencies()
    val choice = readInteger("Select Foreign Currency: ", 1, currencyNames.size)
    if (choice == 1) {
        println("PHP is the base currency. Its rate is always 1.00.")
    } else {
        rates[choice - 1] = readAmount("Exchange Rate (PHP per unit): ", 0.01)
        println("Exchange rate updated.")
    }
}

// Interest is compounded daily, rounding each day's interest before adding it.
fun calculateInterest(balance: Double, days: Int) {
    var projectedBalance = balance
    var day = 1
    while (day <= days) {
        val interest = roundMoney(projectedBalance * (ANNUAL_INTEREST_RATE / 365.0))
        projectedBalance = roundMoney(projectedBalance + interest)
        println("$day | " + formatMoney(interest) + " | " + formatMoney(projectedBalance) + " |")
        day++
    }
}

// Display a daily projection without saving it as an actual deposit.
fun showInterest() {
    println("\nShow Interest Amount")
    val account = selectAccount()
    if (account == null) {
        return
    }
    // Display the same account details for deposits, withdrawals, and interest.
    printAccount(account)
    println("Interest Rate: 5% per annum")
    val days = readInteger("Total Number of Days: ", 1, 36500)
    println("Day | Interest | Balance |")
    calculateInterest(account.balance, days)
}

// Historical reports use the latest valid PHP date, independent of the computer date.
fun reportDate(): LocalDate {
    var latest = LocalDate.now()
    if (transactions.isNotEmpty()) {
        latest = transactions[0].timestamp.toLocalDate()
        for (transaction in transactions) {
            val date = transaction.timestamp.toLocalDate()
            if (date.isAfter(latest)) {
                latest = date
            }
        }
    }
    return latest
}

// Carry the most recent closing balance through every day, including inactive days.
fun averageDailyBalance(account: Account, month: YearMonth): Double {
    var balance = 0.0
    var total = 0.0
    var index = 0
    var day = 1
    while (day <= month.lengthOfMonth()) {
        val date = month.atDay(day)
        while (index < account.transactions.size &&
            !account.transactions[index].timestamp.toLocalDate().isAfter(date)) {
            balance = account.transactions[index].balance
            index++
        }
        total += Math.round(balance * 100.0) // Carry the closing balance through inactive days.
        day++
    }
    return total / (100.0 * month.lengthOfMonth())
}

// Every report uses the same quoted CSV format and a two-row console preview.
fun writeReport(filename: String, headers: List<String>, rows: List<List<String>>) {
    val output = StringBuilder(csvLine(headers) + "\n")
    for (row in rows) {
        output.append(csvLine(row)).append('\n')
    }
    File(filename).writeText(output.toString(), Charsets.UTF_8)
    println(headers.joinToString(" | "))
    var index = 0
    while (index < rows.size && index < 2) {
        println(rows[index].joinToString(" | "))
        index++
    }
    println("${rows.size} rows exported to $filename\n")
}

// Report 1 applies the qualification threshold and tiers to unrounded monthly averages.
fun generatePrivilegeReport(date: LocalDate) {
    val month = YearMonth.from(date).minusMonths(1)
    val results = ArrayList<PrivilegeRow>()
    for (account in accounts.values) {
        val average = averageDailyBalance(account, month)
        if (average >= 100000.0) {
            results.add(PrivilegeRow(account, average))
        }
    }
    results.sort()
    val rows = ArrayList<List<String>>()
    for (result in results) {
        var tier = "Silver"
        if (result.average >= 1000000.0) {
            tier = "Platinum"
        } else if (result.average >= 500000.0) {
            tier = "Gold"
        }
        val age = ChronoUnit.DAYS.between(result.account.opened, date)
        rows.add(listOf(
            result.account.name,
            age.toString(),
            formatMoney(result.account.balance),
            formatMoney(result.average),
            tier
        ))
    }
    println("Report 1: Banking Privilege Tiers ($month)")
    // Write the CSV report and display its first two rows as a preview.
    writeReport("report1_banking_privilege_tiers.csv",
        listOf("AccountName", "AccountAge", "CurrentBalance", "AverageDailyBalance", "PrivilegeTier"), rows)
}

// Project 30 days at the latest balance and round the total interest once.
fun generateInterestReport(): Double {
    val results = ArrayList<InterestRow>()
    for (account in accounts.values) {
        if (account.balance >= 10000.0) {
            val balanceCents = Math.round(account.balance * 100.0)
            val interestCents = balanceCents * 30 / (365.0 / ANNUAL_INTEREST_RATE)
            results.add(InterestRow(account, Math.round(interestCents) / 100.0))
        }
    }
    results.sort()
    val rows = ArrayList<List<String>>()
    var totalInterest = 0.0
    for (result in results) {
        totalInterest += result.interest
        rows.add(listOf(result.account.name, formatMoney(result.account.balance), formatMoney(result.interest)))
    }
    println("Report 2: Interest Expense Projection (30 days)")
    // Write the CSV report and display its first two rows as a preview.
    writeReport("report2_interest_expense_projection.csv",
        listOf("AccountName", "CurrentBalance", "ExpectedInterest"), rows)
    return roundMoney(totalInterest)
}

// Report 3 groups by account number, so clients with the same name stay separate.
fun generateSuspiciousReport() {
    val grouped = TreeMap<String, DailyActivity>()
    for (transaction in transactions) {
        val account = accounts[transaction.accountNumber]
        if (account != null) {
            val date = transaction.timestamp.toLocalDate()
            val key = date.toString() + "|" + account.number
            var activity = grouped[key]
            if (activity == null) {
                activity = DailyActivity(account, date)
                grouped[key] = activity
            }
            if (transaction.type == "deposit") {
                activity.deposits = roundMoney(activity.deposits + transaction.amount)
                activity.depositCount++
            } else {
                activity.withdrawals = roundMoney(activity.withdrawals + transaction.amount)
            }
        }
    }
    val results = ArrayList<DailyActivity>(grouped.values)
    results.sort()
    val rows = ArrayList<List<String>>()
    for (activity in results) {
        var turnover = "N/A" // Division by zero has no defined percentage.
        if (activity.deposits > 0.0 || activity.withdrawals == 0.0) {
            turnover = formatMoney(activity.turnover())
        }
        var risk = "Normal"
        if (activity.isSuspicious()) {
            risk = "Suspicious"
        }
        rows.add(listOf(activity.account.name, activity.date.format(dateFormat), formatMoney(activity.withdrawals),
            activity.depositCount.toString(), formatMoney(activity.deposits), turnover, risk))
    }
    println("Report 3: Suspicious Client Behavior")
    // Write the CSV report and display its first two rows as a preview.
    writeReport("report3_suspicious_client_behavior.csv", listOf("AccountName", "Date", "TotalWithdrawalAmounts",
        "NumberOfDeposits", "TotalDepositAmounts", "TurnoverVelocity (%)", "RiskFlagLevel"), rows)
}

// Report 4 shows funding trends: positive net cash flow adds funds; negative removes them.
fun generateCustomReports(date: LocalDate) {
    val monthOfReport = YearMonth.from(date).minusMonths(1)
    val grouped = TreeMap<String, MonthlyActivity>()
    for (transaction in transactions) {
        val account = accounts[transaction.accountNumber]
        if (account != null) {
            val month = YearMonth.from(transaction.timestamp)
            val key = month.toString() + "|" + account.number
            var activity = grouped[key]
            if (activity == null) {
                activity = MonthlyActivity(account, month)
                grouped[key] = activity
            }
            if (transaction.type == "deposit") {
                activity.depositCount++
                activity.deposits = roundMoney(activity.deposits + transaction.amount)
            } else {
                activity.withdrawalCount++
                activity.withdrawals = roundMoney(activity.withdrawals + transaction.amount)
            }
        }
    }
    val rows = ArrayList<List<String>>()
    for (activity in grouped.values) {
        val net = roundMoney(activity.deposits - activity.withdrawals)
        rows.add(listOf(activity.account.name, activity.account.number, activity.month.toString(),
            formatMoney(activity.deposits), formatMoney(activity.withdrawals), formatMoney(net)))
    }
    println("Report 4: Monthly Net Cash Flow per Account")
    // Write the CSV report and display its first two rows as a preview.
    writeReport("report4_monthly_net_cash_flow.csv", listOf("AccountName", "AccountNumber", "Month",
        "TotalDeposits", "TotalWithdrawals", "NetCashFlow"), rows)

    // Counts distinguish frequent small transactions from a single large movement.
    val activityRows = ArrayList<List<String>>()
    val peakRows = ArrayList<List<String>>()
    val recencyRows = ArrayList<List<String>>()
    for (account in accounts.values) {
        val key = monthOfReport.toString() + "|" + account.number
        var activity = grouped[key]
        if (activity == null) activity = MonthlyActivity(account, monthOfReport)
        activityRows.add(listOf(account.name, account.number, activity.depositCount.toString(),
            activity.withdrawalCount.toString(), formatMoney(activity.deposits), formatMoney(activity.withdrawals),
            formatMoney(roundMoney(activity.deposits - activity.withdrawals))))

        // Largest withdrawal shows each client's peak single-transaction cash demand.
        var peak = 0.0
        for (tx in account.transactions) {
            if (tx.type == "withdrawal" && tx.amount > peak) peak = tx.amount
        }
        peakRows.add(listOf(account.name, account.number, formatMoney(peak)))
        // Days since last use identifies accounts that may need a follow-up.
        val latest = account.transactions.last().timestamp.toLocalDate()
        recencyRows.add(listOf(account.name, account.number, latest.format(dateFormat),
            ChronoUnit.DAYS.between(latest, date).toString()))
    }
    println("Report 4: Monthly Account Activity ($monthOfReport)")
    // Write the CSV report and display its first two rows as a preview.
    writeReport("report4_monthly_account_activity.csv", listOf("AccountName", "AccountNumber", "DepositCount",
        "WithdrawalCount", "TotalDeposits", "TotalWithdrawals", "NetCashFlow"), activityRows)
    println("Report 4: Largest Withdrawal")
    // Write the CSV report and display its first two rows as a preview.
    writeReport("report4_largest_withdrawal.csv", listOf("AccountName", "AccountNumber", "LargestWithdrawal"), peakRows)
    println("Report 4: Account Recency")
    // Write the CSV report and display its first two rows as a preview.
    writeReport("report4_account_recency.csv", listOf("AccountName", "AccountNumber", "LastTransactionDate",
        "DaysSinceLastTransaction"), recencyRows)
}

// Summary totals use all valid PHP history and one latest balance per account.
fun writeSummary(totalInterest: Double) {
    var deposits = 0.0
    var withdrawals = 0.0
    var balance = 0.0
    for (transaction in transactions) {
        if (transaction.type == "deposit") {
            deposits += transaction.amount
        } else {
            withdrawals += transaction.amount
        }
    }
    for (account in accounts.values) {
        balance += account.balance
    }
    val json = "{\n" +
        "  \"total_accounts\": ${accounts.size},\n" +
        "  \"total_deposits\": ${formatPlainMoney(roundMoney(deposits))},\n" +
        "  \"total_withdrawals\": ${formatPlainMoney(roundMoney(withdrawals))},\n" +
        "  \"total_balance\": ${formatPlainMoney(roundMoney(balance))},\n" +
        "  \"total_interest_expense\": ${formatPlainMoney(totalInterest)}\n" +
        "}\n"
    File("summary.json").writeText(json, Charsets.UTF_8)
    println("Summary State (summary.json):\n" + json)
}

// Generate the reports together so the CSV files and summary share one dataset.
fun generateReports(): Boolean {
    if (!dataLoaded) {
        println("Error: Load the file before generating reports.")
        return false
    }
    val date = reportDate()
    println("\nGenerating reports as of " + date.format(dateFormat) + "...\n")
    try {
        generatePrivilegeReport(date)
        val totalInterest = generateInterestReport()
        generateSuspiciousReport()
        generateCustomReports(date)
        // Write summary totals as JSON numbers without thousands separators.
        writeSummary(totalInterest)
        println("All reports saved successfully.")
        return true
    } catch (error: IOException) {
        println("Error: Could not finish writing the reports. " + error.message)
        return false
    }
}

// The report submenu allows the CSV to be reloaded before exporting.
fun reportMenu() {
    var running = true
    while (running) {
        println("\nGenerate Reports")
        println("[1] Load the file")
        println("[2] Generate Reports")
        println("[0] Back")
        val choice = readInteger("Enter choice: ", 0, 2)
        if (choice == 1) {
            // Validate into a temporary dataset before replacing the active accounts.
            loadData()
        } else if (choice == 2) {
            // Generate all reports from one dataset and aggregate the summary totals.
            generateReports()
        } else {
            running = false
        }
    }
}

// Startup loads saved accounts; --reports supports a noninteractive report run.
fun main(args: Array<String>) {
    if (args.size > 1 || (args.size == 1 && args[0] != "--reports")) {
        println("Usage: java -jar MP_8_Kotlin.jar [--reports]")
        kotlin.system.exitProcess(1)
    }
    if (!loadData()) kotlin.system.exitProcess(1)
    if (args.size == 1) {
        if (!generateReports()) kotlin.system.exitProcess(1)
        return
    }
    try {
        var running = true
        while (running) {
            println("\nMain Menu")
            println("Select Transaction:")
            println("[1] Register Account")
            println("[2] Deposit Amount")
            println("[3] Withdraw Amount")
            println("[4] Currency Exchange")
            println("[5] Record Exchange Rates")
            println("[6] Show Interest Amount")
            println("[7] Generate Reports")
            println("[0] Exit")
            val choice = readInteger("Choice: ", 0, 7)
            if (choice == 0) {
                running = false
            } else if (choice == 1) {
                // Assign the next available account number and save its initial deposit.
                registerAccount()
            } else if (choice == 2) {
                // Share deposit and withdrawal prompts while checking the resulting balance.
                transact("deposit")
            } else if (choice == 3) {
                // Share deposit and withdrawal prompts while checking the resulting balance.
                transact("withdrawal")
            } else if (choice == 4) {
                // Convert the source amount through PHP into the selected target currency.
                currencyExchange()
            } else if (choice == 5) {
                // Update a foreign exchange rate for this session while keeping PHP fixed at one.
                recordExchangeRate()
            } else if (choice == 6) {
                // Display daily compounded interest without saving projected deposits.
                showInterest()
            } else if (choice == 7) {
                // Allow repeated reloads and exports until the user selects Back.
                reportMenu()
            }
            if (running) {
                running = askYesNo("Back to the Main Menu (Y/N): ")
            }
        }
    } catch (error: EOFException) {
        println("\nInput ended.")
    }
    println("Thank you for using the application!")
}
