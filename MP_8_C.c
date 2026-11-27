/*
Last Names: Acosta, Cagigas, Pangilinan, Po
Language: C
Paradigm: Procedural programming
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#define CSV_FILENAME "bank_transactions.csv"
#define CSV_HEADER "account_name,account_number,date,time,currency,amount,type,current_balance"
#define ANNUAL_RATE 0.05
#include <math.h>
#include <errno.h>

typedef struct {
    char *name;
    long long number;
    int date, seconds, row, deposit;
    double amount, balance;
} Transaction;

/* Each account owns a consecutive range of rows, sorted by date and time. */
typedef struct {
    int first, end;
    long long number;
    const char *name;
    double average, interest, peak;
} Account;

typedef struct {
    Transaction *rows;
    int count, capacity;
    Account *accounts;
    int accountCount;
    long long highest;
    int latestDate;
} Dataset;

static Dataset data;
static const char *g_currencyNames[] = {
    "",
    "Philippine Peso (PHP)",
    "United States Dollar (USD)",
    "Japanese Yen (JPY)",
    "British Pound Sterling (GBP)",
    "Euro (EUR)",
    "Chinese Yuan Renminbi (CNY)"
};

static double g_rates[] = {0.0, 1.00, 62.00, 0.40, 84.00, 72.00, 9.00};


static void *resize(void *ptr, size_t size) {
    void *result = realloc(ptr, size);
    if (result == NULL) {
        fputs("Out of memory.\n", stderr);
        exit(EXIT_FAILURE);
    }
    return result;
}

static char *copyText(const char *text) {
    char *copy = resize(NULL, strlen(text) + 1);
    return strcpy(copy, text);
}

/* A CSV record occupies one physical line. Grow the buffer for long names. */
static int readLine(FILE *file, char **line, size_t *capacity) {
    int c;
    size_t used = 0;
    while ((c = fgetc(file)) != EOF && c != '\n') {
        if (used + 1 >= *capacity) {
            *capacity = *capacity * 2 + 128;
            *line = resize(*line, *capacity);
        }
        (*line)[used++] = (char)c;
    }
    if (used == 0 && c == EOF) return 0;
    if (*line == NULL) {
        *capacity = 128;
        *line = resize(NULL, *capacity);
    }
    if (used && (*line)[used - 1] == '\r') used--;
    (*line)[used] = '\0';
    return 1;
}

static char *trim(char *s) {
    while (isspace((unsigned char)*s)) s++;
    size_t n = strlen(s);
    while (n && isspace((unsigned char)s[n - 1])) s[--n] = '\0';
    return s;
}

static int sameText(const char *a, const char *b) {
    while (*a && tolower((unsigned char)*a) == tolower((unsigned char)*b)) {
        a++;
        b++;
    }
    return *a == *b;
}

static int digits(const char *s, size_t n) {
    for (size_t i = 0; i < n; i++)
        if (s[i] < '0' || s[i] > '9') return 0;
    return 1;
}

/* Grouping commas must separate groups of three digits. */
static int parseAmount(const char *s, double minimum, double *amount) {
    const char *p = s;
    if (*p == '+' || *p == '-') p++;
    size_t first = strspn(p, "0123456789");
    if (first == 0) return 0;
    p += first;
    if (*p == ',' && first > 3) return 0;
    while (*p == ',') {
        p++;
        if (strspn(p, "0123456789") != 3) return 0;
        p += 3;
    }
    if (*p == '.') {
        p++;
        size_t decimals = strspn(p, "0123456789");
        if (decimals == 0) return 0;
        p += decimals;
    }
    if (*p) return 0;
    char *plain = copyText(s);
    size_t length = 0;
    for (p = s; *p; p++) if (*p != ',') plain[length++] = *p;
    plain[length] = '\0';
    long double value = strtold(plain, NULL);
    if (!(value >= 0 && value < 1.0e12L) || (double)value < minimum) {
        free(plain);
        return 0;
    }
    /* Use the written third decimal to round half-cent inputs consistently. */
    long long cents = strtoll(plain, NULL, 10) * 100;
    const char *fraction = strchr(plain, '.');
    if (fraction) {
        fraction++;
        cents += (fraction[0] - '0') * 10;
        if (fraction[1]) {
            cents += fraction[1] - '0';
            if (fraction[2] >= '5') cents++;
        }
    }
    free(plain);
    *amount = cents / 100.0;
    return 1;
}

// Round monetary results to centavos using half-up rounding.
static double roundCents(double value) {
    double cents = fabs(value) * 100;
    if (cents == floor(cents)) return value;
    return copysign(round(nextafter(cents, INFINITY)) / 100, value);
}

static char *prompt(const char *message) {
    static char *line;
    static size_t capacity;
    printf("%s", message);
    if (!readLine(stdin, &line, &capacity)) {
        puts("\nInput ended.");
        exit(EXIT_SUCCESS);
    }
    return trim(line);
}

// Repeat the prompt until a whole number falls within the permitted range.
static int readInt(const char *message, int min, int max) {
    for (;;) {
        char *s = prompt(message), *end;
        long value = strtol(s, &end, 10);
        if (end != s && !*end && value >= min && value <= max) return (int)value;
        printf("Enter a whole number from %d to %d.\n", min, max);
    }
}

// Check the original amount against the minimum before rounding.
static double readAmount(const char *message, double minimum) {
    for (;;) {
        double value;
        if (parseAmount(prompt(message), minimum, &value) && value >= minimum) return value;
        printf("Enter a valid amount of at least PHP %.2f.\n", minimum);
    }
}

// Accept only Y or N for repeated menu operations.
static int yesNo(const char *message) {
    for (;;) {
        const char *answer = prompt(message);
        if (sameText(answer, "Y")) return 1;
        if (sameText(answer, "N")) return 0;
        puts("Enter Y or N.");
    }
}

void printCurrencyOptions(void) {
    int i;
    for (i = 1; i <= 6; i++) {
        printf("[%d] %s\n", i, g_currencyNames[i]);
    }
}

/* Insert grouping commas in the integer part of a two-decimal amount. */
static void money(double value, char out[64]) {
    char plain[64];
    snprintf(plain, sizeof(plain), "%.2f", roundCents(value));
    const char *digitsStart = plain;
    char *next = out;
    if (*digitsStart == '-') {
        *next++ = '-';
        digitsStart++;
    }
    size_t whole = strspn(digitsStart, "0123456789");
    for (size_t i = 0; i < whole; i++) {
        if (i && (whole - i) % 3 == 0) *next++ = ',';
        *next++ = digitsStart[i];
    }
    strcpy(next, digitsStart + whole);
}

/* Returns 1 if year is a leap year, 0 otherwise. */
int isLeapYear(int year) {
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
}

/* Returns number of days in a given month and year. */
int daysInMonth(int month, int year) {
    static const int days[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    if (month < 1 || month > 12) {
        return 0;
    }
    if (month == 2 && isLeapYear(year)) {
        return 29;
    }
    return days[month];
}

/* Validates a calendar date. */
int isValidDate(int d, int m, int y) {
    if (y < 1 || y > 9999 || m < 1 || m > 12) {
        return 0;
    }
    if (d < 1 || d > daysInMonth(m, y)) {
        return 0;
    }
    return 1;
}

/* Converts a calendar date to Julian Day Number for exact day difference. */
long dateToJDN(int d, int m, int y) {
    int a = (14 - m) / 12;
    int yr = y + 4800 - a;
    int mo = m + 12 * a - 3;
    return d + (153 * mo + 2) / 5 + 365L * yr + yr / 4 - yr / 100 + yr / 400 - 32045;
}


static int parseDate(const char *s) {
    int d, m, y;
    if (strlen(s) != 10 || s[2] != '/' || s[5] != '/' ||
        !digits(s, 2) || !digits(s + 3, 2) || !digits(s + 6, 4)) return 0;
    d = atoi(s);
    m = atoi(s + 3);
    y = atoi(s + 6);
    if (!isValidDate(d, m, y)) return 0;
    return y * 10000 + m * 100 + d;
}

static int parseTime(const char *s) {
    size_t hour = strspn(s, "0123456789");
    if ((hour != 1 && hour != 2) || strlen(s) != hour + 6 || s[hour] != ':' ||
        s[hour + 3] != ':' || !digits(s + hour + 1, 2) || !digits(s + hour + 4, 2)) return -1;
    int h = atoi(s), m = atoi(s + hour + 1), sec = atoi(s + hour + 4);
    if (h > 23 || m > 59 || sec > 59) return -1;
    return h * 3600 + m * 60 + sec;
}

static long dayNumber(int date) {
    return dateToJDN(date % 100, date / 100 % 100, date / 10000);
}

static void dateText(int date, char *out) {
    sprintf(out, "%02d/%02d/%04d", date % 100, date / 100 % 100, date / 10000);
}

/* Decode the eight fields in place, including doubled quotes in quoted names. */
static int splitCsv(char *line, char *fields[]) {
    char *read = line, *write = line;
    int count = 0;
    for (;;) {
        if (count == 8) return 0;
        char *start = write;
        if (*read == '"') {
            read++;
            for (;;) {
                if (!*read) return 0;
                if (*read == '"') {
                    read++;
                    if (*read != '"') break;
                }
                *write++ = *read++;
            }
            while (isspace((unsigned char)*read)) read++;
            if (*read && *read != ',') return 0;
        } else {
            while (*read && *read != ',') {
                if (*read == '"') return 0;
                *write++ = *read++;
            }
        }
        int more = *read == ',';
        if (more) read++;
        *write++ = '\0';
        fields[count++] = trim(start);
        if (!more) break;
    }
    return count == 8;
}

// Quote output fields and escape embedded quotation marks.
static void csvRow(FILE *file, const char *fields[], int count) {
    for (int i = 0; i < count; i++) {
        if (i) fputc(',', file);
        fputc('"', file);
        for (const char *p = fields[i]; *p; p++) {
            if (*p == '"') fputc('"', file);
            fputc(*p, file);
        }
        fputc('"', file);
    }
    fputc('\n', file);
}

static int byNumber(long long a, long long b) {
    if (a < b) return -1;
    return a > b;
}

static int compareTransactions(const void *left, const void *right) {
    const Transaction *a = left, *b = right;
    if (a->number != b->number) return byNumber(a->number, b->number);
    if (a->date != b->date) return a->date - b->date;
    if (a->seconds != b->seconds) return a->seconds - b->seconds;
    return a->row - b->row;
}

static void addTransaction(Dataset *set, Transaction tx) {
    if (set->count == set->capacity) {
        set->capacity = 2 * set->capacity + 128;
        set->rows = resize(set->rows, (size_t)set->capacity * sizeof(tx));
    }
    set->rows[set->count++] = tx;
    if (tx.date > set->latestDate) set->latestDate = tx.date;
}

// Group chronological history by account number and retain the latest balance.
static void buildAccounts(Dataset *set) {
    set->accountCount = 0;
    if (!set->count) return;
    qsort(set->rows, set->count, sizeof(*set->rows), compareTransactions);
    set->accounts = resize(set->accounts, (size_t)set->count * sizeof(Account));
    int i = 0;
    while (i < set->count) {
        Account a = {0};
        a.first = i;
        a.number = set->rows[i].number;
        a.name = set->rows[i].name;
        while (i < set->count && set->rows[i].number == a.number) i++;
        a.end = i;
        set->accounts[set->accountCount++] = a;
    }
}

// Free transaction names and arrays owned by the dataset.
static void releaseDataset(Dataset *set) {
    for (int i = 0; i < set->count; i++) free(set->rows[i].name);
    free(set->rows);
    free(set->accounts);
}

// Validate into a temporary dataset before replacing the active accounts.
static int loadData(void) {
    Dataset next = {0};
    next.highest = -1;
    char *line = NULL, *fields[8];
    size_t capacity = 0;
    int rows = 0, excluded = 0, invalid = 0, ok = 0;
    FILE *fp = fopen(CSV_FILENAME, "rb");
    if (!fp && errno == ENOENT) fp = fopen(CSV_FILENAME, "w+b");
    if (!fp) {
        perror(CSV_FILENAME);
        return 0;
    }
    if (!readLine(fp, &line, &capacity) && feof(fp)) {
        fclose(fp);
        fp = fopen(CSV_FILENAME, "w+b");
        if (!fp) { perror(CSV_FILENAME); return 0; }
        fputs(CSV_HEADER "\n", fp);
        rewind(fp);
        readLine(fp, &line, &capacity);
    }
    char expected[] = CSV_HEADER;
    char *header[8], *start = line;
    if (start && strncmp(start, "\xEF\xBB\xBF", 3) == 0) start += 3;
    if (!start || !splitCsv(start, fields) || !splitCsv(expected, header)) goto done;
    for (int i = 0; i < 8; i++) if (strcmp(fields[i], header[i])) goto done;
    while (readLine(fp, &line, &capacity)) {
        Transaction tx = {0};
        rows++;
        if (!splitCsv(line, fields) || strlen(fields[1]) != 10 || !digits(fields[1], 10)) goto badRow;
        tx.number = strtoll(fields[1], NULL, 10);
        if (tx.number > next.highest) next.highest = tx.number;
        for (int i = 0; i < 8; i++) if (!*fields[i]) goto badRow;
        if (!sameText(fields[4], "PHP")) { excluded++; continue; }
        tx.date = parseDate(fields[2]);
        tx.seconds = parseTime(fields[3]);
        tx.deposit = sameText(fields[6], "deposit");
        if (!tx.date || tx.seconds < 0 || (!tx.deposit && !sameText(fields[6], "withdrawal")) ||
            !parseAmount(fields[5], 0, &tx.amount) || !parseAmount(fields[7], 0, &tx.balance)) goto badRow;
        if (tx.amount <= 0) goto badRow;
        tx.row = rows;
        tx.name = copyText(fields[0]);
        addTransaction(&next, tx);
        continue;
badRow:
        invalid++;
        printf("Row %d: invalid or missing CSV field.\n", rows + 1);
    }
    if (ferror(fp)) goto done;
    // Group chronological history by account number and retain the latest balance.
    buildAccounts(&next);
    if (next.highest < 0) next.highest = 999999999;
    // Free transaction names and arrays owned by the dataset.
    releaseDataset(&data);
    data = next;
    printf("Rows read: %d; valid PHP: %d; non-PHP excluded: %d; invalid rows: %d.\n",
           rows, data.count, excluded, invalid);
    ok = 1;
done:
    if (!ok) {
        puts("Could not load bank_transactions.csv. Check the header and file.");
        // Free transaction names and arrays owned by the dataset.
        releaseDataset(&next);
    }
    free(line);
    fclose(fp);
    return ok;
}

static Transaction *latest(const Account *a) {
    return &data.rows[a->end - 1];
}

// Display the same account details for deposits, withdrawals, and interest.
static void printAccount(const Account *a) {
    char balance[64];
    money(latest(a)->balance, balance);
    printf("Account Name: %s\nAccount Number: %010lld\nCurrent Balance: %s\nCurrency: PHP\n",
           a->name, a->number, balance);
}

static Account *selectAccount(void) {
    if (!data.accountCount) {
        puts("Please register an account first.");
        return NULL;
    }
    for (;;) {
        char *id = prompt("Account Name or Number (0 to cancel): ");
        if (!strcmp(id, "0")) return NULL;
        long long number = -1;
        if (strlen(id) == 10 && digits(id, 10)) number = strtoll(id, NULL, 10);
        /* Accept the sample's "Last, First" lookup as well as the stored name. */
        char *lookup = copyText(id);
        char *comma = strchr(lookup, ',');
        if (comma && !strchr(comma + 1, ',')) {
            *comma = '\0';
            char *reversed = resize(NULL, strlen(id) + 2);
            sprintf(reversed, "%s %s", trim(comma + 1), trim(lookup));
            free(lookup);
            lookup = reversed;
        }
        Account *match = NULL;
        int count = 0;
        for (int i = 0; i < data.accountCount; i++) {
            Account *a = &data.accounts[i];
            if (a->number == number) { free(lookup); return a; }
            if (sameText(a->name, id) || sameText(a->name, lookup)) {
                match = a;
                count++;
            }
        }
        free(lookup);
        if (count == 1) return match;
        if (count > 1) puts("Several accounts have that name. Enter the account number.");
        else puts("Account not found.");
    }
}

// Save the CSV record before updating the in-memory account balance.
static int saveTransaction(const char *name, long long number, int deposit, double amount,
                           double balance, const Account *account) {
    time_t clock = time(NULL);
    struct tm *now = localtime(&clock);
    if (!now || balance >= 1.0e12) { puts("Invalid time or balance."); return 0; }
    Transaction tx = {0};
    tx.date = (now->tm_year + 1900) * 10000 + (now->tm_mon + 1) * 100 + now->tm_mday;
    tx.seconds = now->tm_hour * 3600 + now->tm_min * 60 + now->tm_sec;
    if (account && (latest(account)->date > tx.date ||
        (latest(account)->date == tx.date && latest(account)->seconds > tx.seconds))) {
        puts("This account has a future transaction. Check the dataset or system date.");
        return 0;
    }
    char id[32], date[32], stamp[32], amountText[64], balanceText[64];
    snprintf(id, sizeof(id), "%010lld", number);
    dateText(tx.date, date);
    strftime(stamp, sizeof(stamp), "%H:%M:%S", now);
    snprintf(amountText, sizeof(amountText), "%.2f", amount);
    snprintf(balanceText, sizeof(balanceText), "%.2f", balance);
    const char *type = "withdrawal";
    if (deposit) type = "deposit";
    const char *fields[] = {name, id, date, stamp, "PHP", amountText, type, balanceText};
    FILE *fp = fopen(CSV_FILENAME, "r+b");
    if (!fp) { perror(CSV_FILENAME); return 0; }
    if (fseek(fp, -1, SEEK_END) != 0) {
        fclose(fp);
        puts("Reload the transaction file first.");
        return 0;
    }
    int last = fgetc(fp);
    if (last == EOF || fseek(fp, 0, SEEK_END) != 0) { fclose(fp); return 0; }
    if (last != '\n' && last != '\r') fputc('\n', fp);
    // Quote output fields and escape embedded quotation marks.
    csvRow(fp, fields, 8);
    int failed = ferror(fp);
    if (fclose(fp) != 0 || failed) { perror(CSV_FILENAME); return 0; }

    tx.name = copyText(name);
    tx.number = number;
    tx.deposit = deposit;
    tx.amount = amount;
    tx.balance = balance;
    /* Later file order wins when timestamps match, including after reloading invalid rows. */
    if (account) tx.row = latest(account)->row + 1;
    addTransaction(&data, tx);
    // Group chronological history by account number and retain the latest balance.
    buildAccounts(&data);
    if (number > data.highest) data.highest = number;
    return 1;
}

// Assign the next available account number and save its initial deposit.
static void registerAccount(void) {
    do {
        puts("\nRegister Account");
        if (data.highest >= 9999999999LL) { puts("No more ten-digit account numbers are available."); return; }
        char *name = prompt("Account Name (First name Last name): ");
        while (!strchr(name, ' ')) {
            puts("Enter a first name and last name.");
            name = prompt("Account Name: ");
        }
        name = copyText(name);
        double amount = readAmount("Initial Deposit: ", 5000);
        if (saveTransaction(name, data.highest + 1, 1, amount, amount, NULL)) {
            puts("Account created successfully!");
            // Display the same account details for deposits, withdrawals, and interest.
            printAccount(&data.accounts[data.accountCount - 1]);
        }
        free(name);
    } while (yesNo("Register another account? (Y/N): "));
}

// Share deposit and withdrawal prompts while checking the resulting balance.
static void transact(int deposit) {
    const char *title = "Withdraw Amount", *amountPrompt = "Withdraw Amount: ";
    const char *again = "Withdraw another amount? (Y/N): ";
    if (deposit) {
        title = "Deposit Amount";
        amountPrompt = "Deposit Amount: ";
        again = "Deposit another amount? (Y/N): ";
    }
    do {
        puts(title);
        Account *a = selectAccount();
        if (!a) return;
        // Display the same account details for deposits, withdrawals, and interest.
        printAccount(a);
        double amount = readAmount(amountPrompt, 0.01);
        double balance = latest(a)->balance - amount;
        if (deposit) balance = latest(a)->balance + amount;
        if (balance < 0) puts("Insufficient balance.");
        else if (saveTransaction(a->name, a->number, deposit, amount, roundCents(balance), a)) {
            char text[64];
            money(balance, text);
            printf("Updated Balance: %s\n", text);
        }
    } while (yesNo(again));
}

// Convert the source amount through PHP into the selected target currency.
static void currencyExchange(void) {
    do {
        puts("\nForeign Currency Exchange");
        printCurrencyOptions();
        int from = readInt("Source Currency: ", 1, 6);
        double amount = readAmount("Source Amount: ", 0.01);
        int to = readInt("Exchange Currency: ", 1, 6);
        char result[64];
        money(amount * g_rates[from] / g_rates[to], result);
        printf("Exchange Amount: %s\n", result);
    } while (yesNo("Convert another currency (Y/N)? "));
}

// Update a foreign exchange rate for this session while keeping PHP fixed at one.
static void recordExchangeRate(void) {
    puts("\nRecord Exchange Rate");
    printCurrencyOptions();
    int currency = readInt("Select Foreign Currency: ", 1, 6);
    if (currency == 1) puts("PHP is the base currency. Its rate is always 1.00.");
    else {
        g_rates[currency] = readAmount("Exchange Rate (PHP per unit): ", 0.01);
        puts("Exchange rate updated.");
    }
}

// Display daily compounded interest without saving projected deposits.
static void showInterest(void) {
    puts("\nShow Interest Amount");
    Account *a = selectAccount();
    if (!a) return;
    // Display the same account details for deposits, withdrawals, and interest.
    printAccount(a);
    puts("Interest Rate: 5% per annum");
    int days = readInt("Total Number of Days: ", 1, 36500);
    double balance = latest(a)->balance;
    puts("Day | Interest | Balance |");
    for (int day = 1; day <= days; day++) {
        double interest = roundCents(balance * (ANNUAL_RATE / 365));
        balance = roundCents(balance + interest);
        char earned[64], projected[64];
        money(interest, earned);
        money(balance, projected);
        printf("%d | %s | %s |\n", day, earned, projected);
    }
}

/* Report dates are relative to the latest valid PHP transaction. */
typedef struct {
    Account *account;
    int period, deposits, withdrawals;
    long long depositCents, withdrawalCents;
} Activity;

// Aggregate daily and monthly activity in integer centavos.
static void accumulate(Activity *activity, const Transaction *tx) {
    long long cents = llround(tx->amount * 100);
    if (tx->deposit) { activity->deposits++; activity->depositCents += cents; }
    else { activity->withdrawals++; activity->withdrawalCents += cents; }
}

// Avoid division by zero when a day has no deposits.
static double turnover(const Activity *a) {
    if (!a->depositCents) return 0; // The CSV displays N/A for this case.
    return a->withdrawalCents * 100.0 / a->depositCents;
}

// Apply the withdrawal threshold and turnover threshold to daily totals.
static int suspicious(const Activity *a) {
    return a->withdrawalCents >= 50000000 || turnover(a) >= 90;
}

static int comparePeriod(const void *left, const void *right) {
    const Activity *a = left, *b = right;
    if (a->period != b->period) return a->period - b->period;
    return byNumber(a->account->number, b->account->number);
}

static int compareDay(const void *left, const void *right) {
    const Activity *a = left, *b = right;
    if (a->period != b->period) return a->period - b->period;
    int risk = suspicious(b) - suspicious(a);
    if (risk) return risk;
    return byNumber(a->account->number, b->account->number);
}

static int compareAverage(const void *left, const void *right) {
    const Account *a = left, *b = right;
    if (a->average > b->average) return -1;
    if (a->average < b->average) return 1;
    return byNumber(a->number, b->number);
}

static int compareInterest(const void *left, const void *right) {
    const Account *a = left, *b = right;
    if (a->interest > b->interest) return -1;
    if (a->interest < b->interest) return 1;
    return byNumber(a->number, b->number);
}

// Carry each closing balance through inactive days in the reporting month.
static double averageBalance(const Account *a, int month) {
    int days = daysInMonth(month % 100, month / 100);
    int index = a->first;
    long long sum = 0, closing = 0;
    for (int day = 1; day <= days; day++) {
        while (index < a->end && data.rows[index].date <= month * 100 + day) {
            closing = llround(data.rows[index].balance * 100);
            index++;
        }
        sum += closing; // Include the carried balance on inactive days.
    }
    return (double)sum / (100 * days);
}

typedef struct {
    const char *title, *filename;
    int columns;
    const char *header[7];
    FILE *fp;
    int count;
} Report;

static void outputRow(Report *r, const char *fields[]) {
    // Quote output fields and escape embedded quotation marks.
    csvRow(r->fp, fields, r->columns);
    if (r->count < 2) {
        for (int i = 0; i < r->columns; i++) {
            if (i) printf(" | ");
            printf("%s", fields[i]);
        }
        putchar('\n');
    }
    r->count++;
}

static int openReport(Report *r) {
    r->fp = fopen(r->filename, "wb");
    if (!r->fp) { perror(r->filename); return 0; }
    puts(r->title);
    r->count = -1;
    outputRow(r, r->header);
    return 1;
}

static int closeReport(Report *r) {
    int failed = ferror(r->fp);
    if (fclose(r->fp) != 0 || failed) { perror(r->filename); return 0; }
    printf("%d rows exported to %s\n\n", r->count, r->filename);
    return 1;
}

/* All report fields are quoted UTF-8 with LF line endings. */
static int writePrivileges(Account *rows, int count, int date) {
    Report report = {"Report 1: Banking Privilege Tiers", "report1_banking_privilege_tiers.csv", 5,
        {"AccountName","AccountAge","CurrentBalance","AverageDailyBalance","PrivilegeTier"}, NULL, 0};
    if (!openReport(&report)) return 0;
    qsort(rows, count, sizeof(Account), compareAverage);
    for (int i = 0; i < count; i++) {
        Account *a = &rows[i];
        if (a->average < 100000) break;
        const char *tier = "Silver";
        if (a->average >= 1000000) tier = "Platinum";
        else if (a->average >= 500000) tier = "Gold";
        char age[32], balance[64], average[64];
        snprintf(age, sizeof(age), "%ld", dayNumber(date) - dayNumber(data.rows[a->first].date));
        money(latest(a)->balance, balance);
        money(a->average, average);
        const char *fields[] = {a->name, age, balance, average, tier};
        outputRow(&report, fields);
    }
    return closeReport(&report);
}

static int writeInterest(Account *rows, int count) {
    Report report = {"Report 2: Interest Expense Projection", "report2_interest_expense_projection.csv", 3,
        {"AccountName","CurrentBalance","ExpectedInterest"}, NULL, 0};
    if (!openReport(&report)) return 0;
    qsort(rows, count, sizeof(Account), compareInterest);
    for (int i = 0; i < count; i++) {
        Account *a = &rows[i];
        if (latest(a)->balance < 10000) continue;
        char balance[64], interest[64];
        money(latest(a)->balance, balance);
        money(a->interest, interest);
        const char *fields[] = {a->name, balance, interest};
        outputRow(&report, fields);
    }
    return closeReport(&report);
}

static int writeBehavior(Activity *rows, int count) {
    Report report = {"Report 3: Suspicious Client Behavior", "report3_suspicious_client_behavior.csv", 7,
        {"AccountName","Date","TotalWithdrawalAmounts","NumberOfDeposits","TotalDepositAmounts",
         "TurnoverVelocity (%)","RiskFlagLevel"}, NULL, 0};
    if (!openReport(&report)) return 0;
    qsort(rows, count, sizeof(Activity), compareDay);
    for (int i = 0; i < count; i++) {
        Activity *a = &rows[i];
        char date[32], w[64], d[64], deposits[32], velocity[64] = "N/A";
        dateText(a->period, date);
        snprintf(deposits, sizeof(deposits), "%d", a->deposits);
        money(a->withdrawalCents / 100.0, w);
        money(a->depositCents / 100.0, d);
        if (a->depositCents) money(turnover(a), velocity);
        const char *risk = "Normal";
        if (suspicious(a)) risk = "Suspicious";
        const char *fields[] = {a->account->name, date, w, deposits, d, velocity, risk};
        outputRow(&report, fields);
    }
    return closeReport(&report);
}

/* Net flows show funding trends. Counts show how often a client transacted last month. */
static int writeMonthly(Activity *rows, int count, int countsReport) {
    Report flow = {"Report 4: Monthly Net Cash Flow", "report4_monthly_net_cash_flow.csv", 6,
        {"AccountName","AccountNumber","Month","TotalDeposits","TotalWithdrawals","NetCashFlow"}, NULL, 0};
    Report counts = {"Report 4: Monthly Account Activity", "report4_monthly_account_activity.csv", 7,
        {"AccountName","AccountNumber","DepositCount","WithdrawalCount","TotalDeposits","TotalWithdrawals","NetCashFlow"}, NULL, 0};
    Report *report = &flow;
    if (countsReport) report = &counts;
    if (!openReport(report)) return 0;
    if (!countsReport) qsort(rows, count, sizeof(Activity), comparePeriod);
    for (int i = 0; i < count; i++) {
        Activity *a = &rows[i];
        char number[32], period[32], dc[32], wc[32], deposits[64], withdrawals[64], net[64];
        snprintf(number, sizeof(number), "%010lld", a->account->number);
        money(a->depositCents / 100.0, deposits);
        money(a->withdrawalCents / 100.0, withdrawals);
        money((a->depositCents - a->withdrawalCents) / 100.0, net);
        if (countsReport) {
            snprintf(dc, sizeof(dc), "%d", a->deposits);
            snprintf(wc, sizeof(wc), "%d", a->withdrawals);
            const char *fields[] = {a->account->name, number, dc, wc, deposits, withdrawals, net};
            outputRow(report, fields);
        } else {
            snprintf(period, sizeof(period), "%04d-%02d", a->period / 100, a->period % 100);
            const char *fields[] = {a->account->name, number, period, deposits, withdrawals, net};
            outputRow(report, fields);
        }
    }
    return closeReport(report);
}

/* A peak withdrawal measures single-transaction cash demand. Recency identifies inactivity. */
static int writeAccountHistory(int recency, int date) {
    Report peak = {"Report 4: Largest Withdrawal", "report4_largest_withdrawal.csv", 3,
        {"AccountName","AccountNumber","LargestWithdrawal"}, NULL, 0};
    Report recent = {"Report 4: Account Recency", "report4_account_recency.csv", 4,
        {"AccountName","AccountNumber","LastTransactionDate","DaysSinceLastTransaction"}, NULL, 0};
    Report *report = &peak;
    if (recency) report = &recent;
    if (!openReport(report)) return 0;
    for (int i = 0; i < data.accountCount; i++) {
        const Account *a = &data.accounts[i];
        char number[32], value[64], days[32];
        snprintf(number, sizeof(number), "%010lld", a->number);
        if (recency) {
            dateText(latest(a)->date, value);
            snprintf(days, sizeof(days), "%ld", dayNumber(date) - dayNumber(latest(a)->date));
        } else money(a->peak, value);
        const char *fields[] = {a->name, number, value, days};
        outputRow(report, fields);
    }
    return closeReport(report);
}

// Write summary totals as JSON numbers without thousands separators.
static int writeSummary(double deposits, double withdrawals, double balance, double interest) {
    char json[512];
    snprintf(json, sizeof(json),
        "{\n  \"total_accounts\": %d,\n  \"total_deposits\": %.2f,\n  \"total_withdrawals\": %.2f,\n"
        "  \"total_balance\": %.2f,\n  \"total_interest_expense\": %.2f\n}\n",
        data.accountCount, deposits, withdrawals, balance, interest);
    FILE *fp = fopen("summary.json", "wb");
    if (!fp) { perror("summary.json"); return 0; }
    int failed = fputs(json, fp) == EOF;
    if (fclose(fp) != 0 || failed) { perror("summary.json"); return 0; }
    printf("Summary State (summary.json):\n%s", json);
    return 1;
}

// Generate all reports from one dataset and aggregate the summary totals.
static int generateReports(void) {
    int date = data.latestDate;
    if (!date) {
        time_t clock = time(NULL);
        struct tm *now = localtime(&clock);
        if (!now) return 0;
        date = (now->tm_year + 1900) * 10000 + (now->tm_mon + 1) * 100 + now->tm_mday;
    }
    int month = date / 100 - 1;
    if (date / 100 % 100 == 1) month = (date / 10000 - 1) * 100 + 12;
    char asOf[32];
    dateText(date, asOf);
    printf("Generating reports as of %s...\n", asOf);
    size_t size = (size_t)data.count + 1, accounts = (size_t)data.accountCount + 1;
    Activity *days = resize(NULL, size * sizeof(Activity));
    Activity *months = resize(NULL, size * sizeof(Activity));
    Activity *counts = resize(NULL, accounts * sizeof(Activity));
    Account *ranked = resize(NULL, accounts * sizeof(Account));
    int ndays = 0, nmonths = 0;
    double deposits = 0, withdrawals = 0, balance = 0, interest = 0;
    for (int i = 0; i < data.accountCount; i++) {
        Account *a = &data.accounts[i];
        counts[i] = (Activity){.account = a, .period = month};
        a->average = averageBalance(a, month);
        a->interest = 0;
        a->peak = 0;
        double closing = latest(a)->balance;
        if (closing >= 10000) {
            /* 30 days at 5% annually on an unchanged closing balance. */
            long long balanceCents = llround(closing * 100);
            double interestCents = balanceCents * 30 / (365.0 / ANNUAL_RATE);
            a->interest = round(interestCents) / 100.0;
            interest += a->interest;
        }
        balance += closing;
        for (int j = a->first; j < a->end; j++) {
            const Transaction *tx = &data.rows[j];
            int txMonth = tx->date / 100;
            if (j == a->first || days[ndays - 1].period != tx->date)
                days[ndays++] = (Activity){.account = a, .period = tx->date};
            if (j == a->first || months[nmonths - 1].period != txMonth)
                months[nmonths++] = (Activity){.account = a, .period = txMonth};
            // Aggregate daily and monthly activity in integer centavos.
            accumulate(&days[ndays - 1], tx);
            // Aggregate daily and monthly activity in integer centavos.
            accumulate(&months[nmonths - 1], tx);
            if (txMonth == month) accumulate(&counts[i], tx);
            if (tx->deposit) deposits += tx->amount;
            else {
                withdrawals += tx->amount;
                if (tx->amount > a->peak) a->peak = tx->amount;
            }
        }
        ranked[i] = *a;
    }
    int ok = writePrivileges(ranked, data.accountCount, date) && writeInterest(ranked, data.accountCount)
        && writeBehavior(days, ndays) && writeMonthly(months, nmonths, 0)
        && writeMonthly(counts, data.accountCount, 1) && writeAccountHistory(0, date)
        && writeAccountHistory(1, date) && writeSummary(deposits, withdrawals, balance, interest);
    free(days);
    free(months);
    free(counts);
    free(ranked);
    return ok;
}

// Allow repeated reloads and exports until the user selects Back.
static void reportMenu(void) {
    for (;;) {
        puts("\nGenerate Reports\n[1] Load the file\n[2] Generate Reports\n[0] Back");
        int choice = readInt("Enter choice: ", 0, 2);
        if (!choice) return;
        if (choice == 1) loadData();
        else generateReports();
    }
}

// Load saved accounts and choose batch reports or the interactive menu.
int main(int argc, char **argv) {
    if (argc > 2 || (argc == 2 && strcmp(argv[1], "--reports"))) {
        puts("Usage: bank [--reports]");
        return EXIT_FAILURE;
    }
    if (!loadData()) return EXIT_FAILURE;
    if (argc == 2) {
        int ok = generateReports();
        // Free transaction names and arrays owned by the dataset.
        releaseDataset(&data);
        if (!ok) return EXIT_FAILURE;
        return EXIT_SUCCESS;
    }
    do {
        puts("\nMain Menu\nSelect Transaction:\n[1] Register Account\n[2] Deposit Amount\n[3] Withdraw Amount"
             "\n[4] Currency Exchange\n[5] Record Exchange Rates\n[6] Show Interest Amount\n[7] Generate Reports\n[0] Exit");
        int choice = readInt("Choice: ", 0, 7);
        if (!choice) break;
        switch (choice) {
            case 1: registerAccount(); break;
            case 2: transact(1); break;
            case 3: transact(0); break;
            case 4: currencyExchange(); break;
            case 5: recordExchangeRate(); break;
            case 6: showInterest(); break;
            case 7: reportMenu(); break;
        }
    } while (yesNo("Back to the Main Menu (Y/N): "));
    // Free transaction names and arrays owned by the dataset.
    releaseDataset(&data);
    puts("Thank you for using the application!");
    return EXIT_SUCCESS;
}
