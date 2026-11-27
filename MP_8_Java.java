/*
 * Last Names: Acosta, Cagigas, Pangilinan, Po
 * Language: Java
 * Paradigm(s): Object-Oriented Programming
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
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Scanner;
import java.util.TreeMap;
import java.time.YearMonth;
import java.time.LocalDateTime;
import java.time.format.ResolverStyle;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.NoSuchElementException;
import java.util.regex.Pattern;

public class MP_8_Java {
    private static final Scanner input = new Scanner(System.in);
    private static final String TRANSACTION_FILE = "bank_transactions.csv";
    private static final DateTimeFormatter DATE_FORMAT =
            DateTimeFormatter.ofPattern("dd/MM/uuuu", Locale.US).withResolverStyle(ResolverStyle.STRICT);
    private static final DateTimeFormatter TIME_FORMAT =
            DateTimeFormatter.ofPattern("HH:mm:ss");
    private static final DateTimeFormatter INPUT_TIME =
            DateTimeFormatter.ofPattern("H:mm:ss", Locale.US).withResolverStyle(ResolverStyle.STRICT);
    private static final Pattern INPUT_CLOCK = Pattern.compile("[0-9]{1,2}:[0-9]{2}:[0-9]{2}");
    private static final String HEADER = "account_name,account_number,date,time,currency,amount,type,current_balance";
    private static final Pattern MONEY = Pattern.compile("[+-]?([0-9]+|[0-9]{1,3}(,[0-9]{3})+)(\\.[0-9]+)?");
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

    private static final class Transaction implements Comparable<Transaction> {
        String accountName;
        long accountNumber;
        LocalDate date;
        LocalTime time;
        double amount;
        String type;
        double currentBalance;

        public int compareTo(Transaction other) {
            int order = date.compareTo(other.date);
            if (order != 0) return order;
            return time.compareTo(other.time);
        }

        Transaction(String accountName, long accountNumber, LocalDate date,
                    LocalTime time, double amount,
                    String type, double currentBalance) {
            this.accountName = accountName;
            this.accountNumber = accountNumber;
            this.date = date;
            this.time = time;
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
        double average;
        double expectedInterest;
        final List<Transaction> history = new ArrayList<>();
        LocalDate latestDate;
        LocalTime latestTime;

        Account(Transaction first) {
            accountNumber = first.accountNumber;
            accountName = first.accountName;
            openedDate = first.date;
        }
    }

    private static final class DailyActivity implements Comparable<DailyActivity> {
        String accountName;
        LocalDate date;
        double totalWithdrawals;
        int numberOfDeposits;
        double totalDeposits;

        // Avoid division by zero when a day has no deposits.
        double turnover() {
            if (totalDeposits == 0) return 0;
            return Math.round(totalWithdrawals * 100) * 100.0 / Math.round(totalDeposits * 100);
        }

        // Apply the withdrawal threshold and turnover threshold to daily totals.
        boolean suspicious() {
            return totalWithdrawals >= 500000 || turnover() >= 90;
        }

        public int compareTo(DailyActivity other) {
            int order = date.compareTo(other.date);
            if (order != 0) return order;
            return Boolean.compare(other.suspicious(), suspicious());
        }

        DailyActivity(String accountName, LocalDate date) {
            this.accountName = accountName;
            this.date = date;
        }
    }

    private static final class AccountMonthActivity {
        YearMonth month;
        String accountName;
        long accountNumber;
        int depositCount;
        int withdrawalCount;
        double depositTotal;
        double withdrawalTotal;

        AccountMonthActivity(String accountName, long accountNumber, YearMonth month) {
            this.accountName = accountName;
            this.accountNumber = accountNumber;
            this.month = month;
        }
    }

    private static final class Dataset {
        List<Transaction> transactions = new ArrayList<>();
        Map<Long, Account> accounts = new TreeMap<>();
        int totalRows;
        int phpRows;
        int invalidRows;
        int nonPhpRows;
        long highest = -1;
    }

    private static Dataset dataset = new Dataset();

    // Load saved accounts and choose batch reports or the interactive menu.
    public static void main(String[] args) {
        if (args.length > 1 || (args.length == 1 && !args[0].equals("--reports"))) {
            System.out.println("Usage: java MP_8_Java [--reports]");
            System.exit(1);
        }
        if (!loadDataset()) System.exit(1);
        if (args.length == 1) {
            if (!generateReports()) System.exit(1);
            return;
        }
        try {
            do {
                System.out.println("\nMain Menu\nSelect Transaction:\n[1] Register Account\n[2] Deposit Amount"
                        + "\n[3] Withdraw Amount\n[4] Currency Exchange\n[5] Record Exchange Rates"
                        + "\n[6] Show Interest Amount\n[7] Generate Reports\n[0] Exit");
                switch (readInt("Choice: ", 0, 7)) {
                    case 0: return;
                    case 1: registerAccount(); break;
                    case 2: performTransaction("deposit"); break;
                    case 3: performTransaction("withdrawal"); break;
                    case 4: currencyExchange(); break;
                    case 5: recordExchangeRate(); break;
                    case 6: showInterestAmount(); break;
                    case 7: reportMenu(); break;
                }
            } while (yesNo("Back to the Main Menu (Y/N): "));
        } catch (NoSuchElementException end) {
            System.out.println("\nInput ended.");
        } finally {
            System.out.println("Thank you for using the application!");
        }
    }

    private static String numberText(long number) {
        return String.format(Locale.ROOT, "%010d", number);
    }

    // Display the same account details for deposits, withdrawals, and interest.
    private static void printAccount(Account account) {
        System.out.println("Account Name: " + account.accountName);
        System.out.println("Account Number: " + numberText(account.accountNumber));
        System.out.println("Current Balance: " + money(account.currentBalance) + "\nCurrency: PHP");
    }

    // Find an exact account number or unique name and allow zero to cancel.
    private static Account selectAccount() {
        if (dataset.accounts.isEmpty()) {
            System.out.println("Please register an account first.");
            return null;
        }
        while (true) {
            String id = readLine("Account Name or Number (0 to cancel): ");
            if (id.equals("0")) return null;
            if (id.matches("[0-9]{10}")) {
                Account account = dataset.accounts.get(Long.parseLong(id));
                if (account != null) return account;
            }
            String lookup = id;
            String[] parts = id.split(",", -1);
            if (parts.length == 2) lookup = parts[1].trim() + " " + parts[0].trim();
            Account match = null;
            int count = 0;
            for (Account account : dataset.accounts.values()) {
                if (account.accountName.equalsIgnoreCase(id) || account.accountName.equalsIgnoreCase(lookup)) {
                    match = account;
                    count++;
                }
            }
            if (count == 1) return match;
            if (count > 1) System.out.println("Several accounts have that name. Enter the account number.");
            else System.out.println("Account not found.");
        }
    }

    // Assign the next available account number and save its initial deposit.
    private static void registerAccount() {
        do {
            System.out.println("\nRegister Account");
            if (dataset.highest >= 9999999999L) {
                System.out.println("No more ten-digit account numbers are available.");
                return;
            }
            String name = readLine("Account Name (First name Last name): ");
            while (!name.contains(" ")) {
                System.out.println("Enter a first name and last name.");
                name = readLine("Account Name: ");
            }
            double amount = readAmount("Initial Deposit: ", 5000);
            long number = dataset.highest + 1;
            if (saveTransaction(name, number, amount, "deposit", amount)) {
                System.out.println("Account created successfully!");
                // Display the same account details for deposits, withdrawals, and interest.
                printAccount(dataset.accounts.get(number));
            }
        } while (yesNo("Register another account? (Y/N): "));
    }

    // Share deposit and withdrawal prompts while checking the resulting balance.
    private static void performTransaction(String type) {
        String title = "Deposit Amount";
        String again = "Deposit another amount? (Y/N): ";
        if (type.equals("withdrawal")) {
            title = "Withdraw Amount";
            again = "Withdraw another amount? (Y/N): ";
        }
        do {
            System.out.println("\n" + title);
            Account account = selectAccount();
            if (account == null) return;
            // Display the same account details for deposits, withdrawals, and interest.
            printAccount(account);
            double amount = readAmount(title + ": ", 0.01);
            double balance = account.currentBalance + amount;
            if (type.equals("withdrawal")) balance = account.currentBalance - amount;
            if (balance < 0) System.out.println("Insufficient balance.");
            else if (saveTransaction(account.accountName, account.accountNumber, amount, type, roundMoney(balance))) {
                System.out.println("Updated Balance: " + money(balance));
            }
        } while (yesNo(again));
    }

    // Save the CSV record before updating the in-memory account balance.
    private static boolean saveTransaction(String name, long number, double amount, String type, double balance) {
        LocalDateTime now = LocalDateTime.now().withNano(0);
        Account account = dataset.accounts.get(number);
        if (balance >= 1.0e12 || (account != null && now.isBefore(account.latestDate.atTime(account.latestTime)))) {
            System.out.println("Unable to save: balance is too large or the account has a future transaction.");
            return false;
        }
        Transaction tx = new Transaction(name, number, now.toLocalDate(), now.toLocalTime(), amount, type, balance);
        Path path = Paths.get(TRANSACTION_FILE);
        try (java.io.RandomAccessFile file = new java.io.RandomAccessFile(path.toFile(), "rw")) {
            long length = file.length();
            if (length == 0) throw new IOException("Reload the transaction file first.");
            file.seek(length - 1);
            int last = file.read();
            if (last != '\n' && last != '\r') file.write('\n');
            String row = csvLine(new String[]{name, numberText(number), tx.date.format(DATE_FORMAT),
                    tx.time.format(TIME_FORMAT), "PHP", plain(amount), type, plain(balance)});
            file.write((row + "\n").getBytes(StandardCharsets.UTF_8));
        } catch (IOException error) {
            System.out.println("Transaction could not be saved: " + error.getMessage());
            return false;
        }
        dataset.transactions.add(tx);
        // Group chronological history by account number and retain the latest balance.
        addToAccount(dataset, tx);
        dataset.highest = Math.max(dataset.highest, number);
        return true;
    }

    // Convert the source amount through PHP into the selected target currency.
    private static void currencyExchange() {
        do {
            System.out.println("\nForeign Currency Exchange");
            printCurrencyOptions();
            int source = readInt("Source Currency: ", 1, 6);
            double amount = readAmount("Source Amount: ", 0.01);
            int target = readInt("Exchange Currency: ", 1, 6);
            System.out.println("Exchange Amount: " + money(amount * rates[source] / rates[target]));
        } while (yesNo("Convert another currency (Y/N)? "));
    }

    // Update a foreign exchange rate for this session while keeping PHP fixed at one.
    private static void recordExchangeRate() {
        System.out.println("\nRecord Exchange Rate");
        printCurrencyOptions();
        int target = readInt("Select Foreign Currency: ", 1, 6);
        if (target == 1) System.out.println("PHP is the base currency. Its rate is always 1.00.");
        else {
            rates[target] = readAmount("Exchange Rate (PHP per unit): ", 0.01);
            System.out.println("Exchange rate updated.");
        }
    }

    // Display daily compounded interest without saving projected deposits.
    private static void showInterestAmount() {
        System.out.println("\nShow Interest Amount");
        Account account = selectAccount();
        if (account == null) return;
        // Display the same account details for deposits, withdrawals, and interest.
        printAccount(account);
        System.out.println("Interest Rate: 5% per annum");
        int days = readInt("Total Number of Days: ", 1, 36500);
        double balance = account.currentBalance;
        System.out.println("Day | Interest | Balance |");
        for (int day = 1; day <= days; day++) {
            double interest = roundMoney(balance * (ANNUAL_INTEREST_RATE / 365));
            balance = roundMoney(balance + interest);
            System.out.println(day + " | " + money(interest) + " | " + money(balance) + " |");
        }
    }

    // -------------------- DATA INGESTION AND VALIDATION --------------------

    // Allow repeated reloads and exports until the user selects Back.
    private static void reportMenu() {
        int choice;
        do {
            System.out.println("\nGenerate Reports");
            System.out.println("[1] Load the file");
            System.out.println("[2] Generate Reports");
            System.out.println("[0] Back to Main Menu");
            choice = readInt("Enter choice: ", 0, 2);
            if (choice == 1) loadDataset();
            if (choice == 2) generateReports();
        } while (choice != 0);
    }

    // Group chronological history by account number and retain the latest balance.
    private static void addToAccount(Dataset data, Transaction tx) {
        Account account = data.accounts.get(tx.accountNumber);
        if (account == null) {
            account = new Account(tx);
            data.accounts.put(tx.accountNumber, account);
        }
        account.currentBalance = tx.currentBalance;
        account.latestDate = tx.date;
        account.latestTime = tx.time;
        account.history.add(tx);
    }

    // Validate into a temporary dataset before replacing the active accounts.
    private static boolean loadDataset() {
        Dataset data = new Dataset();
        Path path = Paths.get(TRANSACTION_FILE);
        try {
            if (!Files.exists(path) || Files.size(path) == 0) Files.writeString(path, HEADER + "\n");
            try (BufferedReader reader = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
                String header = reader.readLine();
                if (header == null || !parseCsvLine(header.replaceFirst("^\uFEFF", "")).equals(parseCsvLine(HEADER))) {
                    throw new IOException("Unexpected CSV header.");
                }
                String line;
                while ((line = reader.readLine()) != null) {
                    data.totalRows++;
                    try {
                        List<String> f = parseCsvLine(line);
                        if (f.size() != 8 || !f.get(1).matches("[0-9]{10}"))
                            throw new IllegalArgumentException("Expected eight fields and a ten-digit account number.");
                        long number = Long.parseLong(f.get(1));
                        data.highest = Math.max(data.highest, number);
                        if (f.contains("")) throw new IllegalArgumentException("Missing field.");
                        if (!f.get(4).equalsIgnoreCase("PHP")) {
                            data.nonPhpRows++;
                            continue;
                        }
                        LocalDate date = LocalDate.parse(f.get(2), DATE_FORMAT);
                        if (!INPUT_CLOCK.matcher(f.get(3)).matches()) throw new IllegalArgumentException("Invalid time.");
                        LocalTime time = LocalTime.parse(f.get(3), INPUT_TIME);
                        double amount = roundMoney(parseAmount(f.get(5)));
                        double balance = parseAmount(f.get(7));
                        String type = f.get(6).toLowerCase(Locale.ROOT);
                        if (date.getYear() < 1 || date.getYear() > 9999 || amount <= 0 || balance < 0 ||
                                (!type.equals("deposit") && !type.equals("withdrawal")))
                            throw new IllegalArgumentException("Invalid date, amount, balance or type.");
                        data.transactions.add(new Transaction(f.get(0), number, date, time,
                                amount, type, roundMoney(balance)));
                        data.phpRows++;
                    } catch (RuntimeException error) {
                        data.invalidRows++;
                        System.out.println("Row " + (data.totalRows + 1) + ": " + error.getMessage());
                    }
                }
            }
            data.transactions.sort(null); // Stable: ties stay in file order.
            for (Transaction tx : data.transactions) addToAccount(data, tx);
            if (data.highest < 0) data.highest = 999999999L;
            dataset = data;
            System.out.printf("Rows read: %d; valid PHP: %d; non-PHP excluded: %d; invalid rows: %d.\n",
                    data.totalRows, data.phpRows, data.nonPhpRows, data.invalidRows);
            return true;
        } catch (IOException | IllegalArgumentException error) {
            System.out.println("Could not load " + TRANSACTION_FILE + ": " + error.getMessage());
            return false;
        }
    }

    // -------------------- REPORT GENERATION --------------------

    // Generate all reports from one dataset and aggregate the summary totals.
    private static boolean generateReports() {
        LocalDate date = LocalDate.now();
        if (!dataset.transactions.isEmpty()) {
            date = dataset.transactions.get(0).date;
            for (Transaction tx : dataset.transactions) if (tx.date.isAfter(date)) date = tx.date;
        }
        YearMonth month = YearMonth.from(date).minusMonths(1);
        try {
            System.out.println("Generating reports as of " + date.format(DATE_FORMAT));
            List<Account> tiers = new ArrayList<>(), interest = new ArrayList<>();
            List<String[]> peaks = new ArrayList<>(), recency = new ArrayList<>(), counts = new ArrayList<>();
            Map<String, DailyActivity> days = new TreeMap<>();
            Map<String, AccountMonthActivity> months = new TreeMap<>();
            double deposits = 0, withdrawals = 0, balance = 0, expense = 0;
            for (Account account : dataset.accounts.values()) {
                String name = account.accountName, number = numberText(account.accountNumber);
                double peak = 0;
                for (Transaction tx : account.history) {
                    String dayKey = tx.date + "|" + number;
                    DailyActivity day = days.get(dayKey);
                    if (day == null) {
                        day = new DailyActivity(name, tx.date);
                        days.put(dayKey, day);
                    }
                    YearMonth txMonth = YearMonth.from(tx.date);
                    String monthKey = txMonth + "|" + number;
                    AccountMonthActivity activity = months.get(monthKey);
                    if (activity == null) {
                        activity = new AccountMonthActivity(name, account.accountNumber, txMonth);
                        months.put(monthKey, activity);
                    }
                    if (tx.type.equals("deposit")) {
                        day.totalDeposits = roundMoney(day.totalDeposits + tx.amount);
                        day.numberOfDeposits++;
                        activity.depositTotal = roundMoney(activity.depositTotal + tx.amount);
                        activity.depositCount++;
                        deposits += tx.amount;
                    } else {
                        day.totalWithdrawals = roundMoney(day.totalWithdrawals + tx.amount);
                        activity.withdrawalTotal = roundMoney(activity.withdrawalTotal + tx.amount);
                        activity.withdrawalCount++;
                        withdrawals += tx.amount;
                        peak = Math.max(peak, tx.amount);
                    }
                }
                account.average = averageDailyBalance(account, month);
                if (account.average >= 100000) tiers.add(account);
                balance += account.currentBalance;
                if (account.currentBalance >= 10000) {
                    // Constant closing balance times 5% / 365 for 30 days, rounded once.
                    long balanceCents = Math.round(account.currentBalance * 100);
                    double interestCents = balanceCents * 30 / (365.0 / ANNUAL_INTEREST_RATE);
                    account.expectedInterest = Math.round(interestCents) / 100.0;
                    expense += account.expectedInterest;
                    interest.add(account);
                }
                // Frequency distinguishes many small movements from one large transfer.
                AccountMonthActivity activity = months.get(month + "|" + number);
                if (activity == null) activity = new AccountMonthActivity(name, account.accountNumber, month);
                counts.add(new String[]{name, number, Integer.toString(activity.depositCount),
                        Integer.toString(activity.withdrawalCount), money(activity.depositTotal), money(activity.withdrawalTotal),
                        money(roundMoney(activity.depositTotal - activity.withdrawalTotal))});
                // Peak withdrawals measure single-transaction demand. Recency identifies inactive accounts.
                peaks.add(new String[]{name, number, money(peak)});
                recency.add(new String[]{name, number, account.latestDate.format(DATE_FORMAT),
                        Long.toString(ChronoUnit.DAYS.between(account.latestDate, date))});
            }
            tiers.sort(MP_8_Java::compareAverage);
            List<String[]> rows = new ArrayList<>();
            for (Account account : tiers) {
                String tier = "Silver";
                if (account.average >= 1000000) tier = "Platinum";
                else if (account.average >= 500000) tier = "Gold";
                rows.add(new String[]{account.accountName, Long.toString(ChronoUnit.DAYS.between(account.openedDate, date)),
                        money(account.currentBalance), money(account.average), tier});
            }
            // Write the CSV report and display its first two rows as a preview.
            export("Banking Privilege Tiers", "report1_banking_privilege_tiers.csv",
                    new String[]{"AccountName","AccountAge","CurrentBalance","AverageDailyBalance","PrivilegeTier"}, rows);
            interest.sort(MP_8_Java::compareInterest);
            rows.clear();
            for (Account account : interest)
                rows.add(new String[]{account.accountName, money(account.currentBalance), money(account.expectedInterest)});
            // Write the CSV report and display its first two rows as a preview.
            export("Interest Expense Projection", "report2_interest_expense_projection.csv",
                    new String[]{"AccountName","CurrentBalance","ExpectedInterest"}, rows);
            List<DailyActivity> ordered = new ArrayList<>(days.values());
            ordered.sort(null);
            rows.clear();
            for (DailyActivity day : ordered) {
                String risk = "Normal", velocity = "N/A";
                if (day.suspicious()) risk = "Suspicious";
                if (day.totalDeposits > 0) velocity = money(day.turnover());
                rows.add(new String[]{day.accountName, day.date.format(DATE_FORMAT), money(day.totalWithdrawals),
                        Integer.toString(day.numberOfDeposits), money(day.totalDeposits), velocity, risk});
            }
            // Write the CSV report and display its first two rows as a preview.
            export("Suspicious Client Behavior", "report3_suspicious_client_behavior.csv",
                    new String[]{"AccountName","Date","TotalWithdrawalAmounts","NumberOfDeposits","TotalDepositAmounts",
                            "TurnoverVelocity (%)","RiskFlagLevel"}, rows);
            rows.clear();
            // Net cash flow shows which months added to or drew down account funding.
            for (AccountMonthActivity m : months.values()) {
                rows.add(new String[]{m.accountName, numberText(m.accountNumber), m.month.toString(),
                        money(m.depositTotal), money(m.withdrawalTotal), money(roundMoney(m.depositTotal - m.withdrawalTotal))});
            }
            // Write the CSV report and display its first two rows as a preview.
            export("Monthly Net Cash Flow", "report4_monthly_net_cash_flow.csv",
                    new String[]{"AccountName","AccountNumber","Month","TotalDeposits","TotalWithdrawals","NetCashFlow"}, rows);
            // Write the CSV report and display its first two rows as a preview.
            export("Monthly Account Activity", "report4_monthly_account_activity.csv",
                    new String[]{"AccountName","AccountNumber","DepositCount","WithdrawalCount","TotalDeposits","TotalWithdrawals","NetCashFlow"}, counts);
            // Write the CSV report and display its first two rows as a preview.
            export("Largest Withdrawal", "report4_largest_withdrawal.csv",
                    new String[]{"AccountName","AccountNumber","LargestWithdrawal"}, peaks);
            // Write the CSV report and display its first two rows as a preview.
            export("Account Recency", "report4_account_recency.csv",
                    new String[]{"AccountName","AccountNumber","LastTransactionDate","DaysSinceLastTransaction"}, recency);
            // Write summary totals as JSON numbers without thousands separators.
            writeSummaryJson(dataset.accounts.size(), deposits, withdrawals, balance, expense);
            return true;
        } catch (IOException error) {
            System.out.println("Could not finish writing reports: " + error.getMessage());
            return false;
        }
    }

    // Carry each closing balance through inactive days in the reporting month.
    private static double averageDailyBalance(Account account, YearMonth month) {
        double sumCents = 0;
        long closingCents = 0;
        int index = 0;
        for (int day = 1; day <= month.lengthOfMonth(); day++) {
            LocalDate end = month.atDay(day);
            while (index < account.history.size() && !account.history.get(index).date.isAfter(end)) {
                closingCents = Math.round(account.history.get(index).currentBalance * 100);
                index++;
            }
            sumCents += closingCents; // Include inactive days in the monthly average.
        }
        return sumCents / (100 * month.lengthOfMonth());
    }

    private static int compareAverage(Account a, Account b) {
        return Double.compare(b.average, a.average); // Highest average comes first.
    }

    private static int compareInterest(Account a, Account b) {
        return Double.compare(b.expectedInterest, a.expectedInterest);
    }

    // Quote output fields and escape embedded quotation marks.
    private static String csvLine(String[] fields) {
        List<String> escaped = new ArrayList<>();
        for (String field : fields) escaped.add("\"" + field.replace("\"", "\"\"") + "\"");
        return String.join(",", escaped);
    }

    // Write the CSV report and display its first two rows as a preview.
    private static void export(String title, String filename, String[] header, List<String[]> rows) throws IOException {
        try (BufferedWriter writer = Files.newBufferedWriter(Paths.get(filename), StandardCharsets.UTF_8)) {
            writer.write(csvLine(header) + "\n");
            for (String[] row : rows) writer.write(csvLine(row) + "\n");
        }
        System.out.println(title + "\n" + String.join(" | ", header));
        for (int i = 0; i < rows.size() && i < 2; i++) System.out.println(String.join(" | ", rows.get(i)));
        System.out.println(rows.size() + " rows exported to " + filename);
    }

    // Write summary totals as JSON numbers without thousands separators.
    private static void writeSummaryJson(int totalAccounts, double totalDeposits,
            double totalWithdrawals, double totalBalance,
            double totalInterestExpense) throws IOException {
        String json = String.format(Locale.US,
                "{\n"
                + "  \"total_accounts\": %d,\n"
                + "  \"total_deposits\": %.2f,\n"
                + "  \"total_withdrawals\": %.2f,\n"
                + "  \"total_balance\": %.2f,\n"
                + "  \"total_interest_expense\": %.2f\n"
                + "}\n",
                totalAccounts, totalDeposits, totalWithdrawals,
                totalBalance, totalInterestExpense);

        System.out.println("Summary State (summary.json):\n" + json);
        Files.write(Paths.get("summary.json"),
                json.getBytes(StandardCharsets.UTF_8));
    }

    private static void printCurrencyOptions() {
        for (int i = 1; i <= 6; i++) {
            System.out.printf("[%d] %s\n", i, currencyNames[i]);
        }
    }

    private static String readLine(String prompt) {
        System.out.print(prompt);
        return input.nextLine().trim();
    }

    // Repeat the prompt until a whole number falls within the permitted range.
    private static int readInt(String prompt, int min, int max) {
        while (true) {
            try {
                int value = Integer.parseInt(readLine(prompt));
                if (value >= min && value <= max) return value;
            } catch (NumberFormatException ignored) {
                // Retry below.
            }
            System.out.printf("Enter a whole number from %d to %d.%n", min, max);
        }
    }

    // Validate numeric syntax and amount limits before accepting financial input.
    private static double parseAmount(String text) {
        if (!MONEY.matcher(text).matches()) throw new NumberFormatException("Invalid financial value.");
        double value = Double.parseDouble(text.replace(",", ""));
        if (!Double.isFinite(value) || value >= 1.0e12) throw new NumberFormatException("Amount too large.");
        return value;
    }

    // Check the original amount against the minimum before rounding.
    private static double readAmount(String prompt, double minimum) {
        while (true) {
            try {
                double value = parseAmount(readLine(prompt));
                if (value >= minimum && roundMoney(value) >= minimum) return roundMoney(value);
            } catch (NumberFormatException ignored) {
                // Retry below.
            }
            System.out.println("Enter a valid amount of at least PHP " + money(minimum) + ".");
        }
    }

    // Accept only Y or N for repeated menu operations.
    private static boolean yesNo(String prompt) {
        while (true) {
            String answer = readLine(prompt);
            if (answer.equalsIgnoreCase("Y")) return true;
            if (answer.equalsIgnoreCase("N")) return false;
            System.out.println("Enter Y or N.");
        }
    }

    // Round monetary results to centavos using half-up rounding.
    private static double roundMoney(double amount) {
        return BigDecimal.valueOf(amount).setScale(2, RoundingMode.HALF_UP).doubleValue();
    }

    private static String plain(double amount) {
        return String.format(Locale.US, "%.2f", amount);
    }

    private static String money(double amount) {
        return String.format(Locale.US, "%,.2f", amount);
    }

    private static List<String> parseCsvLine(String line) {
        List<String> fields = new ArrayList<>();
        StringBuilder field = new StringBuilder();
        boolean quoted = false, closed = false;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (quoted) {
                if (c != '"') field.append(c);
                else if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    field.append('"');
                    i++;
                } else {
                    quoted = false;
                    closed = true;
                }
            } else if (c == ',') {
                fields.add(field.toString().trim());
                field.setLength(0);
                closed = false;
            } else if (closed) {
                if (!Character.isWhitespace(c)) throw new IllegalArgumentException("Text after quoted field.");
            } else if (c == '"') {
                if (field.length() > 0) throw new IllegalArgumentException("Unexpected quote.");
                quoted = true;
            } else field.append(c);
        }
        if (quoted) throw new IllegalArgumentException("Unclosed quoted field.");
        fields.add(field.toString().trim());
        return fields;
    }
}
