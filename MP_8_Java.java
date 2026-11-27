/*
 * MCO2 - Banking Deposit and Withdrawal Reports
 * Last Names: Acosta, Cagigas, Pangilinan, Po
 * Language: Java
 * Paradigm(s): Object-Oriented Programming
 *
 * Compile: javac MP_8_Java.java
 * Run:     java MP_8_Java
 */

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Scanner;
import java.util.TreeMap;
import java.util.stream.Collectors;

public class MP_8_Java {
    private static final Scanner input = new Scanner(System.in);
    private static final String TRANSACTION_FILE = "bank_transactions.csv";
    private static final DateTimeFormatter DATE_FORMAT =
            DateTimeFormatter.ofPattern("dd/MM/yyyy");
    private static final DateTimeFormatter TIME_FORMAT =
            DateTimeFormatter.ofPattern("HH:mm:ss");
    private static final double ANNUAL_INTEREST_RATE = 0.05;

    private static final String[] currencyNames = {
        "",
        "Philippine Peso (PHP)",
        "United States Dollar (USD)",
        "Japanese Yen (JPY)",
        "British Pound Sterling (GBP)",
        "Euro (EUR)",
        "Chinese Yuan Renminbi (CNY)"
    };

    // Rates are expressed as PHP value of one unit of each currency.
    private static final double[] rates = {
        0.0, 1.00, 62.00, 0.40, 84.00, 72.00, 9.00
    };

    private static final class Transaction {
        String accountName;
        long accountNumber;
        LocalDate date;
        LocalTime time;
        String currency;
        double amount;
        String type;
        double currentBalance;

        Transaction(String accountName, long accountNumber, LocalDate date,
                    LocalTime time, String currency, double amount,
                    String type, double currentBalance) {
            this.accountName = accountName;
            this.accountNumber = accountNumber;
            this.date = date;
            this.time = time;
            this.currency = currency;
            this.amount = amount;
            this.type = type;
            this.currentBalance = currentBalance;
        }
    }

    private static final class Account {
        long accountNumber;
        String accountName;
        LocalDate openedDate;
        double currentBalance;
        LocalDate latestDate;
        LocalTime latestTime;

        Account(long accountNumber, String accountName, LocalDate openedDate,
                double currentBalance, LocalDate latestDate, LocalTime latestTime) {
            this.accountNumber = accountNumber;
            this.accountName = accountName;
            this.openedDate = openedDate;
            this.currentBalance = currentBalance;
            this.latestDate = latestDate;
            this.latestTime = latestTime;
        }
    }

    private static final class DailyActivity {
        String accountName;
        LocalDate date;
        double totalWithdrawals;
        int numberOfDeposits;
        double totalDeposits;

        DailyActivity(String accountName, LocalDate date) {
            this.accountName = accountName;
            this.date = date;
        }
    }

    private static final class AccountMonthActivity {
        String accountName;
        long accountNumber;
        int depositCount;
        int withdrawalCount;
        double depositTotal;
        double withdrawalTotal;

        AccountMonthActivity(String accountName, long accountNumber) {
            this.accountName = accountName;
            this.accountNumber = accountNumber;
        }
    }

    private static final class Dataset {
        List<Transaction> transactions = new ArrayList<>();
        Map<Long, Account> accounts = new LinkedHashMap<>();
        int totalRows;
        int phpRows;
        int invalidRows;
        int nonPhpRows;
        boolean loaded;
    }

    private static final Dataset dataset = new Dataset();

    public static void main(String[] args) {
        int choice;

        do {
            System.out.println("\nSelect Transaction:");
            System.out.println("[1] Register Account");
            System.out.println("[2] Deposit Amount");
            System.out.println("[3] Withdraw Amount");
            System.out.println("[4] Currency Exchange");
            System.out.println("[5] Record Exchange Rates");
            System.out.println("[6] Show Interest Amount");
            System.out.println("[7] Generate Reports");
            System.out.println("[0] Exit");
            System.out.print("\nChoice: ");

            choice = readInt();

            switch (choice) {
                case 1:
                    registerAccount();
                    break;
                case 2:
                    performTransaction("deposit");
                    break;
                case 3:
                    performTransaction("withdrawal");
                    break;
                case 4:
                    currencyExchange();
                    break;
                case 5:
                    recordExchangeRate();
                    break;
                case 6:
                    showInterestAmount();
                    break;
                case 7:
                    reportMenu();
                    break;
                case 0:
                    System.out.println("Exiting program...");
                    break;
                default:
                    System.out.println("Invalid choice. Please try again.");
                    break;
            }
        } while (choice != 0);

        input.close();
    }

    // -------------------- ACCOUNT AND TRANSACTION FEATURES --------------------

    private static void registerAccount() {
        System.out.println("\nRegister Account");
        System.out.print("Account Name (First name Last name): ");
        String name = input.nextLine().trim();

        if (name.isEmpty() || name.split("\\s+").length < 2) {
            System.out.println("Please enter a first name and last name.");
            return;
        }

        System.out.print("Initial Deposit (minimum Php 5,000.00): ");
        double initialDeposit = readDouble();

        if (!Double.isFinite(initialDeposit) || initialDeposit < 5000.00) {
            System.out.println("Initial deposit must be at least Php 5,000.00.");
            return;
        }

        try {
            long nextNumber = getNextAccountNumber();
            LocalDate today = LocalDate.now();
            LocalTime now = LocalTime.now().withNano(0);

            Transaction transaction = new Transaction(
                    name, nextNumber, today, now, "PHP",
                    initialDeposit, "deposit", initialDeposit
            );

            appendTransaction(transaction);
            System.out.println("\nAccount created successfully!");
            System.out.println("Account Name: " + name);
            System.out.println("Account Number: " + nextNumber);
            System.out.printf(Locale.US, "Current Balance: %.2f%n", initialDeposit);
        } catch (IOException e) {
            System.out.println("Unable to save the account: " + e.getMessage());
        }
    }

    private static long getNextAccountNumber() throws IOException {
        long highest = 1092000000L;

        if (Files.exists(Paths.get(TRANSACTION_FILE))) {
            try (BufferedReader reader = Files.newBufferedReader(
                    Paths.get(TRANSACTION_FILE), StandardCharsets.UTF_8)) {
                String line;
                boolean firstLine = true;

                while ((line = reader.readLine()) != null) {
                    if (firstLine) {
                        firstLine = false;
                        if (line.toLowerCase(Locale.ROOT).contains("account_number")) {
                            continue;
                        }
                    }

                    List<String> fields = parseCsvLine(line);
                    if (fields.size() > 1) {
                        try {
                            highest = Math.max(highest,
                                    Long.parseLong(fields.get(1).trim()));
                        } catch (NumberFormatException ignored) {
                            // Invalid account numbers are skipped.
                        }
                    }
                }
            }
        }

        return highest + 1;
    }

    private static void performTransaction(String transactionType) {
        System.out.println("\n" + (transactionType.equals("deposit")
                ? "Deposit Amount" : "Withdraw Amount"));

        Account account = selectAccount();
        if (account == null) {
            return;
        }

        System.out.println("Account Name: " + account.accountName);
        System.out.println("Account Number: " + account.accountNumber);
        System.out.printf(Locale.US, "Current Balance: %.2f%n", account.currentBalance);
        System.out.println("Currency: PHP");
        System.out.print((transactionType.equals("deposit")
                ? "Deposit Amount: " : "Withdraw Amount: "));

        double amount = readDouble();
        if (!Double.isFinite(amount) || amount <= 0) {
            System.out.println("Enter a valid amount greater than zero.");
            return;
        }

        if (transactionType.equals("withdrawal") && amount > account.currentBalance) {
            System.out.println("Insufficient balance.");
            return;
        }

        double updatedBalance = transactionType.equals("deposit")
                ? account.currentBalance + amount
                : account.currentBalance - amount;

        Transaction transaction = new Transaction(
                account.accountName, account.accountNumber,
                LocalDate.now(), LocalTime.now().withNano(0),
                "PHP", amount, transactionType, updatedBalance
        );

        try {
            appendTransaction(transaction);
            System.out.printf(Locale.US, "Updated Balance: %.2f%n", updatedBalance);
            System.out.println("Transaction saved successfully.");
        } catch (IOException e) {
            System.out.println("Unable to save transaction: " + e.getMessage());
        }
    }

    private static Account selectAccount() {
        Dataset current = loadDataset(false);
        if (!current.loaded || current.accounts.isEmpty()) {
            System.out.println("No accounts were found. Register an account first.");
            return null;
        }

        System.out.print("Enter Account Number: ");
        long accountNumber = readLong();
        Account account = current.accounts.get(accountNumber);

        if (account == null) {
            System.out.println("Account number not found.");
        }

        return account;
    }

    private static void appendTransaction(Transaction transaction) throws IOException {
        Path path = Paths.get(TRANSACTION_FILE);
        boolean needsHeader = !Files.exists(path) || Files.size(path) == 0;

        try (BufferedWriter writer = Files.newBufferedWriter(
                path, StandardCharsets.UTF_8,
                java.nio.file.StandardOpenOption.CREATE,
                java.nio.file.StandardOpenOption.APPEND)) {
            if (needsHeader) {
                writer.write("account_name,account_number,date,time,currency,amount,type,current_balance");
                writer.newLine();
            }

            writer.write(csv(transaction.accountName) + ","
                    + transaction.accountNumber + ","
                    + transaction.date.format(DATE_FORMAT) + ","
                    + transaction.time.format(TIME_FORMAT) + ","
                    + transaction.currency + ","
                    + String.format(Locale.US, "%.2f", transaction.amount) + ","
                    + transaction.type + ","
                    + String.format(Locale.US, "%.2f", transaction.currentBalance));
            writer.newLine();
        }
    }

    // -------------------- MCO1 CURRENCY AND INTEREST FEATURES --------------------

    private static void currencyExchange() {
        System.out.println("\nForeign Currency Exchange");
        System.out.println("Source Currency Option:");
        printCurrencyOptions();
        System.out.print("\nSource Currency: ");
        int source = readInt();

        if (source < 1 || source > 6) {
            System.out.println("Invalid currency.");
            return;
        }

        System.out.print("Source Amount: ");
        double amount = readDouble();
        if (!Double.isFinite(amount) || amount < 0) {
            System.out.println("Invalid amount.");
            return;
        }

        System.out.println("\nExchanged Currency Options:");
        printCurrencyOptions();
        System.out.print("\nExchange Currency: ");
        int destination = readInt();

        if (destination < 1 || destination > 6) {
            System.out.println("Invalid currency.");
            return;
        }

        double converted = amount * rates[source] / rates[destination];
        System.out.printf(Locale.US, "Exchange Amount: %.2f %s%n",
                converted, currencyCode(destination));
    }

    private static void recordExchangeRate() {
        System.out.println("\nRecord Exchange Rate");
        printCurrencyOptions();
        System.out.print("\nSelect Foreign Currency: ");
        int currencyIndex = readInt();

        if (currencyIndex < 2 || currencyIndex > 6) {
            System.out.println("Invalid currency. Select a foreign currency.");
            return;
        }

        System.out.print("Exchange Rate (PHP value of one unit): ");
        double newRate = readDouble();

        if (!Double.isFinite(newRate) || newRate <= 0) {
            System.out.println("Enter a valid exchange rate.");
            return;
        }

        rates[currencyIndex] = newRate;
        System.out.println("Exchange rate updated successfully.");
    }

    private static void showInterestAmount() {
        System.out.println("\nShow Interest Amount");
        Account account = selectAccount();
        if (account == null) {
            return;
        }

        System.out.println("Account Name: " + account.accountName);
        System.out.printf(Locale.US, "Current Balance: %.2f%n", account.currentBalance);
        System.out.println("Currency: PHP");
        System.out.println("Interest Rate: 5% per annum");
        System.out.print("Total Number of Days: ");
        int days = readInt();

        if (days <= 0) {
            System.out.println("Enter a number of days greater than zero.");
            return;
        }

        double temporaryBalance = account.currentBalance;
        System.out.println("\nDay\t| Interest\t| Balance");
        for (int day = 1; day <= days; day++) {
            double dailyInterest = temporaryBalance * (ANNUAL_INTEREST_RATE / 365.0);
            double roundedInterest = Math.round(dailyInterest * 100.0) / 100.0;
            temporaryBalance += roundedInterest;

            System.out.printf(Locale.US, "%d\t| %.2f\t\t| %.2f%n",
                    day, roundedInterest, temporaryBalance);
        }
    }

    // -------------------- DATA INGESTION AND VALIDATION --------------------

    private static void reportMenu() {
        int choice;
        do {
            System.out.println("\nGenerate Reports");
            System.out.println("[1] Load the file");
            System.out.println("[2] Generate Reports");
            System.out.println("[0] Back to Main Menu");
            System.out.print("Enter choice: ");
            choice = readInt();

            switch (choice) {
                case 1:
                    loadDataset(true);
                    break;
                case 2:
                    if (!dataset.loaded) {
                        System.out.println("Please load the CSV file first.");
                    } else {
                        generateReports();
                    }
                    break;
                case 0:
                    break;
                default:
                    System.out.println("Invalid choice.");
                    break;
            }
        } while (choice != 0);
    }

    private static Dataset loadDataset(boolean showMessage) {
        Dataset loadedData = new Dataset();
        Path path = Paths.get(TRANSACTION_FILE);

        if (!Files.exists(path)) {
            if (showMessage) {
                System.out.println("File not found: " + TRANSACTION_FILE);
            }
            return loadedData;
        }

        try (BufferedReader reader = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            String line;
            boolean firstLine = true;

            while ((line = reader.readLine()) != null) {
                if (firstLine) {
                    firstLine = false;
                    if (line.toLowerCase(Locale.ROOT).contains("account_name")) {
                        continue;
                    }
                }

                loadedData.totalRows++;
                try {
                    List<String> fields = parseCsvLine(line);
                    if (fields.size() != 8) {
                        throw new IllegalArgumentException("Expected 8 columns.");
                    }

                    String name = fields.get(0).trim();
                    long number = Long.parseLong(fields.get(1).trim());
                    LocalDate date = LocalDate.parse(fields.get(2).trim(), DATE_FORMAT);
                    LocalTime time = parseTime(fields.get(3).trim());
                    String currency = fields.get(4).trim().toUpperCase(Locale.ROOT);
                    double amount = parseMoney(fields.get(5));
                    String type = fields.get(6).trim().toLowerCase(Locale.ROOT);
                    double balance = parseMoney(fields.get(7));

                    if (name.isEmpty() || currency.isEmpty()
                            || ( !type.equals("deposit") && !type.equals("withdrawal"))) {
                        throw new IllegalArgumentException("Missing or invalid field.");
                    }

                    if (!currency.equals("PHP")) {
                        loadedData.nonPhpRows++;
                        continue;
                    }

                    if (amount < 0 || balance < 0) {
                        throw new IllegalArgumentException("Negative financial value.");
                    }

                    Transaction transaction = new Transaction(
                            name, number, date, time, currency, amount, type, balance);
                    loadedData.transactions.add(transaction);
                    loadedData.phpRows++;

                    Account account = loadedData.accounts.get(number);
                    if (account == null) {
                        loadedData.accounts.put(number, new Account(
                                number, name, date, balance, date, time));
                    } else {
                        if (date.isBefore(account.openedDate)) {
                            account.openedDate = date;
                        }
                        if (date.isAfter(account.latestDate)
                                || (date.equals(account.latestDate)
                                && time.isAfter(account.latestTime))) {
                            account.latestDate = date;
                            account.latestTime = time;
                            account.currentBalance = balance;
                            account.accountName = name;
                        }
                    }
                } catch (RuntimeException e) {
                    loadedData.invalidRows++;
                }
            }

            loadedData.transactions.sort(Comparator
                    .comparing((Transaction t) -> t.date)
                    .thenComparing(t -> t.time));

            loadedData.loaded = true;
            if (showMessage) {
                System.out.println("\nProcessing dataset...");
                System.out.println("Total data rows: " + loadedData.totalRows);
                System.out.println("PHP transactions loaded: " + loadedData.phpRows);
                System.out.println("Non-PHP rows filtered: " + loadedData.nonPhpRows);
                System.out.println("Invalid rows skipped: " + loadedData.invalidRows);
                System.out.println("Accounts found: " + loadedData.accounts.size());
            }

            // Keep the loaded dataset available to the report menu.
            dataset.transactions = loadedData.transactions;
            dataset.accounts = loadedData.accounts;
            dataset.totalRows = loadedData.totalRows;
            dataset.phpRows = loadedData.phpRows;
            dataset.invalidRows = loadedData.invalidRows;
            dataset.nonPhpRows = loadedData.nonPhpRows;
            dataset.loaded = true;
            return dataset;

        } catch (IOException e) {
            if (showMessage) {
                System.out.println("Error reading CSV file: " + e.getMessage());
            }
            return loadedData;
        }
    }

    private static LocalTime parseTime(String value) {
        try {
            // The supplied CSV uses both padded and unpadded hour values.
            return LocalTime.parse(value, DateTimeFormatter.ofPattern("H:mm:ss"));
        } catch (DateTimeParseException e) {
            try {
                return LocalTime.parse(value, TIME_FORMAT);
            } catch (DateTimeParseException secondError) {
                // Also accept times that omit seconds, if present in another file.
                return LocalTime.parse(value, DateTimeFormatter.ofPattern("H:mm"));
            }
        }
    }

    private static double parseMoney(String value) {
        return Double.parseDouble(value.trim().replace(",", ""));
    }

    // -------------------- REPORT GENERATION --------------------

    private static void generateReports() {
        if (!dataset.loaded) {
            System.out.println("Load the data before generating reports.");
            return;
        }

        LocalDate today = LocalDate.now();
        LocalDate firstDayOfLastMonth = today.withDayOfMonth(1).minusMonths(1);
        LocalDate lastDayOfLastMonth = today.withDayOfMonth(1).minusDays(1);

        try {
            List<String[]> report1 = createPrivilegeReport(
                    firstDayOfLastMonth, lastDayOfLastMonth);
            List<String[]> report2 = createInterestReport();
            List<String[]> report3 = createSuspiciousReport();
            List<String[]> report4 = createCustomReport(
                    firstDayOfLastMonth, lastDayOfLastMonth);

            writeCsv("report1_banking_privilege_tiers.csv",
                    new String[]{"AccountName", "AccountAge", "CurrentBalance",
                            "AverageDailyBalance", "PrivilegeTier"}, report1);

            writeCsv("report2_interest_expense_projection.csv",
                    new String[]{"AccountName", "CurrentBalance", "ExpectedInterest"}, report2);

            writeCsv("report3_suspicious_client_behavior.csv",
                    new String[]{"AccountName", "Date", "TotalWithdrawalAmounts",
                            "NumberOfDeposits", "TotalDepositAmounts",
                            "TurnoverVelocity", "RiskFlagLevel"}, report3);

            writeCsv("report4_monthly_account_activity.csv",
                    new String[]{"AccountName", "AccountNumber", "DepositCount",
                            "WithdrawalCount", "TotalDeposits", "TotalWithdrawals",
                            "NetCashFlow"}, report4);

            double totalDeposits = 0.0;
            double totalWithdrawals = 0.0;
            for (Transaction transaction : dataset.transactions) {
                if (transaction.type.equals("deposit")) {
                    totalDeposits += transaction.amount;
                } else {
                    totalWithdrawals += transaction.amount;
                }
            }

            double totalBalance = dataset.accounts.values().stream()
                    .mapToDouble(account -> account.currentBalance).sum();
            double totalInterest = report2.stream()
                    .mapToDouble(row -> parseFormattedMoney(row[2])).sum();

            writeSummaryJson(dataset.accounts.size(), totalDeposits,
                    totalWithdrawals, totalBalance, totalInterest);

            System.out.println("\nGenerating reports...");
            System.out.println("Outputs saved to individual files.");
            previewReport("Report 1: Banking Privilege Tiers", report1,
                    "report1_banking_privilege_tiers.csv");
            previewReport("Report 2: Interest Expense Projection", report2,
                    "report2_interest_expense_projection.csv");
            previewReport("Report 3: Suspicious Client Behavior", report3,
                    "report3_suspicious_client_behavior.csv");
            previewReport("Report 4: Monthly Account Activity", report4,
                    "report4_monthly_account_activity.csv");

            System.out.println("\nSummary State (summary.json):");
            System.out.printf(Locale.US,
                    "{\"total_accounts\": %d, \"total_deposits\": %.2f, "
                    + "\"total_withdrawals\": %.2f, \"total_balance\": %.2f, "
                    + "\"total_interest_expense\": %.2f}%n",
                    dataset.accounts.size(), totalDeposits, totalWithdrawals,
                    totalBalance, totalInterest);

        } catch (IOException e) {
            System.out.println("Unable to generate reports: " + e.getMessage());
        }
    }

    private static List<String[]> createPrivilegeReport(
            LocalDate firstDay, LocalDate lastDay) {
        List<String[]> rows = new ArrayList<>();
        int daysInMonth = lastDay.getDayOfMonth();

        Map<Long, Map<LocalDate, Double>> dailyBalances = new LinkedHashMap<>();
        for (Account account : dataset.accounts.values()) {
            dailyBalances.put(account.accountNumber, new TreeMap<>());
        }

        Map<Long, List<Transaction>> accountTransactions = groupByAccount();
        for (Map.Entry<Long, List<Transaction>> entry : accountTransactions.entrySet()) {
            List<Transaction> transactions = entry.getValue();
            Map<LocalDate, Double> balances = dailyBalances.get(entry.getKey());

            for (Transaction transaction : transactions) {
                if (!transaction.date.isAfter(lastDay)) {
                    balances.put(transaction.date, transaction.currentBalance);
                }
            }
        }

        for (Account account : dataset.accounts.values()) {
            Map<LocalDate, Double> balances = dailyBalances.get(account.accountNumber);
            double startingBalance = 0.0;

            // Carry forward the most recent balance before the report month.
            for (Map.Entry<LocalDate, Double> entry : balances.entrySet()) {
                if (entry.getKey().isBefore(firstDay)) {
                    startingBalance = entry.getValue();
                }
            }

            double sumOfDailyBalances = 0.0;
            double currentDayBalance = startingBalance;

            for (int day = 1; day <= daysInMonth; day++) {
                LocalDate date = firstDay.withDayOfMonth(day);
                if (balances.containsKey(date)) {
                    currentDayBalance = balances.get(date);
                }
                sumOfDailyBalances += currentDayBalance;
            }

            double averageDailyBalance = sumOfDailyBalances / daysInMonth;
            if (averageDailyBalance >= 100000.00) {
                String tier;
                if (averageDailyBalance < 500000.00) {
                    tier = "Silver";
                } else if (averageDailyBalance < 1000000.00) {
                    tier = "Gold";
                } else {
                    tier = "Platinum";
                }

                long age = Math.max(0,
                        ChronoUnit.DAYS.between(account.openedDate, LocalDate.now()));
                rows.add(new String[]{
                    account.accountName,
                    String.valueOf(age),
                    money(account.currentBalance),
                    money(averageDailyBalance),
                    tier
                });
            }
        }

        rows.sort((a, b) -> Double.compare(
                parseFormattedMoney(b[3]), parseFormattedMoney(a[3])));
        return rows;
    }

    private static List<String[]> createInterestReport() {
        List<String[]> rows = new ArrayList<>();

        for (Account account : dataset.accounts.values()) {
            if (account.currentBalance >= 10000.00) {
                double dailyInterest = account.currentBalance
                        * (ANNUAL_INTEREST_RATE / 365.0);
                double expectedInterest = dailyInterest * 30.0;

                rows.add(new String[]{
                    account.accountName,
                    money(account.currentBalance),
                    money(expectedInterest)
                });
            }
        }

        rows.sort((a, b) -> Double.compare(
                parseFormattedMoney(b[2]), parseFormattedMoney(a[2])));
        return rows;
    }

    private static List<String[]> createSuspiciousReport() {
        Map<String, DailyActivity> activities = new LinkedHashMap<>();

        for (Transaction transaction : dataset.transactions) {
            String key = transaction.accountNumber + "|" + transaction.date;
            DailyActivity activity = activities.get(key);

            if (activity == null) {
                activity = new DailyActivity(transaction.accountName, transaction.date);
                activities.put(key, activity);
            }

            if (transaction.type.equals("withdrawal")) {
                activity.totalWithdrawals += transaction.amount;
            } else if (transaction.type.equals("deposit")) {
                activity.numberOfDeposits++;
                activity.totalDeposits += transaction.amount;
            }
        }

        List<DailyActivity> sortedActivities = new ArrayList<>(activities.values());
        sortedActivities.sort(Comparator.comparing(activity -> activity.date));

        List<String[]> rows = new ArrayList<>();
        for (DailyActivity activity : sortedActivities) {
            double turnover = activity.totalDeposits == 0.0
                    ? (activity.totalWithdrawals > 0.0 ? 100.0 : 0.0)
                    : activity.totalWithdrawals / activity.totalDeposits * 100.0;

            String risk = (activity.totalWithdrawals >= 500000.00 || turnover >= 90.0)
                    ? "Suspicious" : "Normal";

            rows.add(new String[]{
                activity.accountName,
                activity.date.format(DATE_FORMAT),
                money(activity.totalWithdrawals),
                String.valueOf(activity.numberOfDeposits),
                money(activity.totalDeposits),
                money(turnover),
                risk
            });
        }

        rows.sort((a, b) -> {
            LocalDate dateA = LocalDate.parse(a[1], DATE_FORMAT);
            LocalDate dateB = LocalDate.parse(b[1], DATE_FORMAT);
            int dateComparison = dateA.compareTo(dateB);
            if (dateComparison != 0) {
                return dateComparison;
            }
            return b[6].compareTo(a[6]); // Suspicious before Normal.
        });

        return rows;
    }

    /*
     * Customized Report 4:
     * Monthly Account Activity summarizes each client's deposit and withdrawal
     * counts, total amounts, and net cash flow for the same reporting month.
     * This helps identify accounts with unusually high activity and whether
     * money is generally entering or leaving each account.
     */
    private static List<String[]> createCustomReport(
            LocalDate firstDay, LocalDate lastDay) {
        Map<Long, AccountMonthActivity> activityMap = new LinkedHashMap<>();

        for (Account account : dataset.accounts.values()) {
            activityMap.put(account.accountNumber,
                    new AccountMonthActivity(account.accountName, account.accountNumber));
        }

        for (Transaction transaction : dataset.transactions) {
            if (!transaction.date.isBefore(firstDay)
                    && !transaction.date.isAfter(lastDay)) {
                AccountMonthActivity activity = activityMap.get(transaction.accountNumber);
                if (activity != null) {
                    if (transaction.type.equals("deposit")) {
                        activity.depositCount++;
                        activity.depositTotal += transaction.amount;
                    } else if (transaction.type.equals("withdrawal")) {
                        activity.withdrawalCount++;
                        activity.withdrawalTotal += transaction.amount;
                    }
                }
            }
        }

        List<String[]> rows = new ArrayList<>();
        for (AccountMonthActivity activity : activityMap.values()) {
            double netCashFlow = activity.depositTotal - activity.withdrawalTotal;
            rows.add(new String[]{
                activity.accountName,
                String.valueOf(activity.accountNumber),
                String.valueOf(activity.depositCount),
                String.valueOf(activity.withdrawalCount),
                money(activity.depositTotal),
                money(activity.withdrawalTotal),
                money(netCashFlow)
            });
        }

        rows.sort((a, b) -> Double.compare(
                parseFormattedMoney(b[6]), parseFormattedMoney(a[6])));
        return rows;
    }

    private static Map<Long, List<Transaction>> groupByAccount() {
        Map<Long, List<Transaction>> grouped = new LinkedHashMap<>();
        for (Transaction transaction : dataset.transactions) {
            grouped.computeIfAbsent(transaction.accountNumber,
                    key -> new ArrayList<>()).add(transaction);
        }
        return grouped;
    }

    // -------------------- OUTPUT FILES --------------------

    private static void writeCsv(String fileName, String[] headers,
                                 List<String[]> rows) throws IOException {
        try (BufferedWriter writer = Files.newBufferedWriter(
                Paths.get(fileName), StandardCharsets.UTF_8)) {
            writer.write(String.join(",", headers));
            writer.newLine();

            for (String[] row : rows) {
                List<String> escaped = new ArrayList<>();
                for (String value : row) {
                    escaped.add(csv(value));
                }
                writer.write(String.join(",", escaped));
                writer.newLine();
            }
        }
    }

    private static void writeSummaryJson(int totalAccounts, double totalDeposits,
            double totalWithdrawals, double totalBalance,
            double totalInterestExpense) throws IOException {
        String json = String.format(Locale.US,
                "{%n"
                + "  \"total_accounts\": %d,%n"
                + "  \"total_deposits\": %.2f,%n"
                + "  \"total_withdrawals\": %.2f,%n"
                + "  \"total_balance\": %.2f,%n"
                + "  \"total_interest_expense\": %.2f%n"
                + "}%n",
                totalAccounts, totalDeposits, totalWithdrawals,
                totalBalance, totalInterestExpense);

        Files.write(Paths.get("summary.json"),
                json.getBytes(StandardCharsets.UTF_8));
    }

    private static void previewReport(String title, List<String[]> rows,
                                      String fileName) {
        System.out.println("\n" + title);
        if (rows.isEmpty()) {
            System.out.println("No matching records.");
        } else {
            for (int i = 0; i < Math.min(2, rows.size()); i++) {
                System.out.println(String.join(" | ", rows.get(i)));
            }
        }
        System.out.println("(Full table exported to " + fileName + ")");
    }

    // -------------------- INPUT, CSV, AND FORMATTING HELPERS --------------------

    private static void printCurrencyOptions() {
        for (int i = 1; i <= 6; i++) {
            System.out.printf("[%d] %s%n", i, currencyNames[i]);
        }
    }

    private static String currencyCode(int index) {
        String[] codes = {"", "PHP", "USD", "JPY", "GBP", "EUR", "CNY"};
        return codes[index];
    }

    private static int readInt() {
        String line = input.nextLine().trim();
        try {
            return Integer.parseInt(line);
        } catch (NumberFormatException e) {
            return Integer.MIN_VALUE;
        }
    }

    private static long readLong() {
        String line = input.nextLine().trim();
        try {
            return Long.parseLong(line);
        } catch (NumberFormatException e) {
            return Long.MIN_VALUE;
        }
    }

    private static double readDouble() {
        String line = input.nextLine().trim();
        try {
            return Double.parseDouble(line.replace(",", ""));
        } catch (NumberFormatException e) {
            return Double.NaN;
        }
    }

    private static String money(double amount) {
        return String.format(Locale.US, "%,.2f", amount);
    }

    private static double parseFormattedMoney(String value) {
        return Double.parseDouble(value.replace(",", ""));
    }

    private static String csv(String value) {
        if (value == null) {
            return "";
        }
        if (value.contains(",") || value.contains("\"")
                || value.contains("\n") || value.contains("\r")) {
            return "\"" + value.replace("\"", "\"\"") + "\"";
        }
        return value;
    }

    // Handles commas and escaped quotes inside quoted CSV fields.
    private static List<String> parseCsvLine(String line) {
        List<String> fields = new ArrayList<>();
        StringBuilder field = new StringBuilder();
        boolean insideQuotes = false;

        for (int i = 0; i < line.length(); i++) {
            char character = line.charAt(i);

            if (character == '"') {
                if (insideQuotes && i + 1 < line.length()
                        && line.charAt(i + 1) == '"') {
                    field.append('"');
                    i++;
                } else {
                    insideQuotes = !insideQuotes;
                }
            } else if (character == ',' && !insideQuotes) {
                fields.add(field.toString());
                field.setLength(0);
            } else {
                field.append(character);
            }
        }

        fields.add(field.toString());
        return fields;
    }
}
