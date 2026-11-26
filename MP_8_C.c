/*
Last Names: Acosta, Cagigas, Pangilinan, Po
Language: C
Paradigm(s): Procedural Programming
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define CSV_FILENAME "bank_transactions.csv"
#define MAX_TXNS 20000
#define MAX_ACCOUNTS 2000
#define MAX_DAILY_RECS 20000
#define MAX_LINE 512

typedef struct {
    char accountName[64];
    long long accountNumber;
    int day;
    int month;
    int year;
    int hour;
    int minute;
    int second;
    char currency[16];
    double amount;
    char type[16];
    double currentBalance;
} Transaction;

typedef struct {
    char accountName[64];
    long long accountNumber;
    int firstDay;
    int firstMonth;
    int firstYear;
    int lastDay;
    int lastMonth;
    int lastYear;
    int lastSecOfDay;
    double currentBalance;
    double avgDailyBalance;
    double expectedInterest;
    int accountAge;
    char privilegeTier[16];
} AccountSummary;

typedef struct {
    char accountName[64];
    int day;
    int month;
    int year;
    double totalWithdrawals;
    int numDeposits;
    double totalDeposits;
    double turnoverVelocity;
    char riskFlag[16];
} DailyBehavior;

static Transaction g_txns[MAX_TXNS];
static int g_txnCount = 0;
static int g_totalRowsRead = 0;
static int g_invalidRows = 0;
static int g_dataLoaded = 0;
static long long g_maxAccountNumber = 1092000000LL;

static AccountSummary g_accounts[MAX_ACCOUNTS];
static int g_accountCount = 0;

static AccountSummary g_report1[MAX_ACCOUNTS];
static int g_report1Count = 0;

static AccountSummary g_report2[MAX_ACCOUNTS];
static int g_report2Count = 0;

static DailyBehavior g_report3[MAX_DAILY_RECS];
static int g_report3Count = 0;

static char g_activeAccountName[64] = "None";
static double g_activeBalance = 1000.00;

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

/* Clears leftover characters in the standard input buffer. */
void clearInputBuffer(void) {
    int c;
    while ((c = getchar()) != '\n' && c != EOF) {
    }
}

/* Prints the 6 supported currency options. */
void printCurrencyOptions(void) {
    int i;
    for (i = 1; i <= 6; i++) {
        printf("[%d] %s\n", i, g_currencyNames[i]);
    }
}

/* Formats a double with comma separators and 2 decimal places into outBuf. */
void formatCommaNumber(double value, char *outBuf, size_t bufSize) {
    char raw[64];
    char intPart[64];
    char decPart[16];
    char formatted[64];
    char *dot;
    int isNeg = 0;
    int len;
    int i, j, count;

    if (value < 0.0) {
        isNeg = 1;
        value = -value;
    }

    snprintf(raw, sizeof(raw), "%.2f", value);
    dot = strchr(raw, '.');
    if (dot != NULL) {
        size_t intLen = (size_t)(dot - raw);
        strncpy(intPart, raw, intLen);
        intPart[intLen] = '\0';
        snprintf(decPart, sizeof(decPart), "%s", dot);
    } else {
        snprintf(intPart, sizeof(intPart), "%s", raw);
        decPart[0] = '\0';
    }

    len = (int)strlen(intPart);
    j = 0;
    count = 0;
    for (i = len - 1; i >= 0; i--) {
        if (count > 0 && count % 3 == 0) {
            formatted[j++] = ',';
        }
        formatted[j++] = intPart[i];
        count++;
    }
    if (isNeg) {
        formatted[j++] = '-';
    }
    formatted[j] = '\0';

    for (i = 0; i < j / 2; i++) {
        char tmp = formatted[i];
        formatted[i] = formatted[j - 1 - i];
        formatted[j - 1 - i] = tmp;
    }

    snprintf(outBuf, bufSize, "%s%s", formatted, decPart);
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
    if (y < 1900 || y > 2100 || m < 1 || m > 12) {
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

/* Compares two dates: returns -1, 0, or 1. */
int compareDates(int d1, int m1, int y1, int d2, int m2, int y2) {
    if (y1 != y2) return (y1 < y2) ? -1 : 1;
    if (m1 != m2) return (m1 < m2) ? -1 : 1;
    if (d1 != d2) return (d1 < d2) ? -1 : 1;
    return 0;
}

/* Finds an account index by name, or -1 if not found. */
int findAccountByName(const char *name) {
    int i;
    for (i = 0; i < g_accountCount; i++) {
        if (strcmp(g_accounts[i].accountName, name) == 0) {
            return i;
        }
    }
    return -1;
}

/* Builds the unique accounts table from loaded transactions. */
void rebuildAccountSummaries(void) {
    int i, idx;
    g_accountCount = 0;

    for (i = 0; i < g_txnCount; i++) {
        Transaction *t = &g_txns[i];
        int sec = t->hour * 3600 + t->minute * 60 + t->second;
        idx = findAccountByName(t->accountName);

        if (idx == -1) {
            if (g_accountCount >= MAX_ACCOUNTS) {
                continue;
            }
            idx = g_accountCount++;
            snprintf(g_accounts[idx].accountName, sizeof(g_accounts[idx].accountName), "%s", t->accountName);
            g_accounts[idx].accountNumber = t->accountNumber;
            g_accounts[idx].firstDay = t->day;
            g_accounts[idx].firstMonth = t->month;
            g_accounts[idx].firstYear = t->year;
            g_accounts[idx].lastDay = t->day;
            g_accounts[idx].lastMonth = t->month;
            g_accounts[idx].lastYear = t->year;
            g_accounts[idx].lastSecOfDay = sec;
            g_accounts[idx].currentBalance = t->currentBalance;
        } else {
            int cmpFirst = compareDates(t->day, t->month, t->year,
                                        g_accounts[idx].firstDay, g_accounts[idx].firstMonth, g_accounts[idx].firstYear);
            int cmpLast = compareDates(t->day, t->month, t->year,
                                       g_accounts[idx].lastDay, g_accounts[idx].lastMonth, g_accounts[idx].lastYear);
            if (cmpFirst < 0) {
                g_accounts[idx].firstDay = t->day;
                g_accounts[idx].firstMonth = t->month;
                g_accounts[idx].firstYear = t->year;
            }
            if (cmpLast > 0 || (cmpLast == 0 && sec >= g_accounts[idx].lastSecOfDay)) {
                g_accounts[idx].lastDay = t->day;
                g_accounts[idx].lastMonth = t->month;
                g_accounts[idx].lastYear = t->year;
                g_accounts[idx].lastSecOfDay = sec;
                g_accounts[idx].currentBalance = t->currentBalance;
            }
        }
    }
}

/* Loads, validates, and cleans transactions from bank_transactions.csv. */
int loadTransactionsFromCSV(int verbose) {
    FILE *fp = fopen(CSV_FILENAME, "r");
    char line[MAX_LINE];
    int phpCount = 0;

    if (fp == NULL) {
        if (verbose) {
            printf("Error: Could not open %s.\n", CSV_FILENAME);
        }
        return 0;
    }

    g_txnCount = 0;
    g_totalRowsRead = 0;
    g_invalidRows = 0;

    if (fgets(line, sizeof(line), fp) == NULL) {
        fclose(fp);
        return 0;
    }

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *fields[8];
        int fieldIdx = 0;
        char *p = line;
        char *newline;
        int d, m, y, hh, mm, ss;
        long long accNum;
        double amt, bal;
        char *endPtr;

        newline = strpbrk(line, "\r\n");
        if (newline != NULL) {
            *newline = '\0';
        }
        if (line[0] == '\0') {
            continue;
        }

        g_totalRowsRead++;

        while (fieldIdx < 8) {
            fields[fieldIdx++] = p;
            p = strchr(p, ',');
            if (p == NULL) {
                break;
            }
            *p = '\0';
            p++;
        }

        if (fieldIdx < 8) {
            g_invalidRows++;
            continue;
        }

        if (fields[0][0] == '\0' || fields[1][0] == '\0' || fields[2][0] == '\0' ||
            fields[3][0] == '\0' || fields[4][0] == '\0' || fields[5][0] == '\0' ||
            fields[6][0] == '\0' || fields[7][0] == '\0') {
            g_invalidRows++;
            continue;
        }

        accNum = strtoll(fields[1], &endPtr, 10);
        if (*endPtr != '\0') {
            g_invalidRows++;
            continue;
        }
        if (accNum > g_maxAccountNumber) {
            g_maxAccountNumber = accNum;
        }

        if (sscanf(fields[2], "%d/%d/%d", &d, &m, &y) != 3 || !isValidDate(d, m, y)) {
            g_invalidRows++;
            continue;
        }

        if (sscanf(fields[3], "%d:%d:%d", &hh, &mm, &ss) != 3 ||
            hh < 0 || hh > 23 || mm < 0 || mm > 59 || ss < 0 || ss > 59) {
            g_invalidRows++;
            continue;
        }

        amt = strtod(fields[5], &endPtr);
        if (*endPtr != '\0') {
            g_invalidRows++;
            continue;
        }

        bal = strtod(fields[7], &endPtr);
        if (*endPtr != '\0') {
            g_invalidRows++;
            continue;
        }

        if (strcmp(fields[4], "PHP") != 0) {
            continue;
        }

        if (g_txnCount < MAX_TXNS) {
            Transaction *t = &g_txns[g_txnCount++];
            snprintf(t->accountName, sizeof(t->accountName), "%s", fields[0]);
            t->accountNumber = accNum;
            t->day = d;
            t->month = m;
            t->year = y;
            t->hour = hh;
            t->minute = mm;
            t->second = ss;
            snprintf(t->currency, sizeof(t->currency), "%s", fields[4]);
            t->amount = amt;
            snprintf(t->type, sizeof(t->type), "%s", fields[6]);
            t->currentBalance = bal;
            phpCount++;
        }
    }

    fclose(fp);
    g_dataLoaded = 1;
    rebuildAccountSummaries();

    if (verbose) {
        char totalBuf[32], phpBuf[32];
        formatCommaNumber((double)g_totalRowsRead, totalBuf, sizeof(totalBuf));
        formatCommaNumber((double)phpCount, phpBuf, sizeof(phpBuf));
        totalBuf[strlen(totalBuf) - 3] = '\0';
        phpBuf[strlen(phpBuf) - 3] = '\0';
        printf("Processing dataset... (%s rows loaded, %s filtered for PHP currency", totalBuf, phpBuf);
        if (g_invalidRows > 0) {
            printf(", %d invalid rows skipped", g_invalidRows);
        }
        printf(")\n");
    }

    return 1;
}

/* Appends a single transaction row to bank_transactions.csv. */
int appendTransactionToCSV(const char *accName, long long accNum, const char *type, double amount, double newBalance) {
    FILE *fp = fopen(CSV_FILENAME, "a");
    time_t now = time(NULL);
    struct tm *t = localtime(&now);
    char dateStr[32];
    char timeStr[32];

    if (fp == NULL) {
        printf("Error: Could not write to %s.\n", CSV_FILENAME);
        return 0;
    }

    snprintf(dateStr, sizeof(dateStr), "%02d/%02d/%04d", t->tm_mday, t->tm_mon + 1, t->tm_year + 1900);
    snprintf(timeStr, sizeof(timeStr), "%02d:%02d:%02d", t->tm_hour, t->tm_min, t->tm_sec);

    fprintf(fp, "%s,%lld,%s,%s,PHP,%.2f,%s,%.2f\n",
            accName, accNum, dateStr, timeStr, amount, type, newBalance);
    fclose(fp);

    if (g_dataLoaded && g_txnCount < MAX_TXNS) {
        Transaction *tx = &g_txns[g_txnCount++];
        snprintf(tx->accountName, sizeof(tx->accountName), "%s", accName);
        tx->accountNumber = accNum;
        tx->day = t->tm_mday;
        tx->month = t->tm_mon + 1;
        tx->year = t->tm_year + 1900;
        tx->hour = t->tm_hour;
        tx->minute = t->tm_min;
        tx->second = t->tm_sec;
        snprintf(tx->currency, sizeof(tx->currency), "PHP");
        tx->amount = amount;
        snprintf(tx->type, sizeof(tx->type), "%s", type);
        tx->currentBalance = newBalance;
        rebuildAccountSummaries();
    }

    return 1;
}

/* Menu [1]: Register Account */
void handleRegisterAccount(void) {
    char another;
    if (!g_dataLoaded) {
        loadTransactionsFromCSV(0);
    }

    do {
        char name[64];
        double initDeposit = 0.0;

        printf("\nRegister Account\n");
        printf("Account Name: ");
        scanf(" %63[^\n]", name);

        printf("Initial Deposit: ");
        if (scanf("%lf", &initDeposit) != 1 || initDeposit < 5000.00) {
            clearInputBuffer();
            printf("Invalid deposit. Minimum initial deposit is Php. 5,000.00.\n");
        } else {
            g_maxAccountNumber++;
            if (appendTransactionToCSV(name, g_maxAccountNumber, "deposit", initDeposit, initDeposit)) {
                snprintf(g_activeAccountName, sizeof(g_activeAccountName), "%s", name);
                g_activeBalance = initDeposit;
                printf("Account created successfully! (Account Number: %lld)\n", g_maxAccountNumber);
            }
        }

        printf("\nRegister another account? (Y/N): ");
        scanf(" %c", &another);
    } while (another == 'Y' || another == 'y');
}

/* Menu [2]: Deposit Amount */
void handleDepositAmount(void) {
    char another;
    if (!g_dataLoaded) {
        loadTransactionsFromCSV(0);
    }

    do {
        char name[64];
        double deposit = 0.0;
        int idx;

        printf("\nDeposit Amount\n");
        printf("Account Name: ");
        scanf(" %63[^\n]", name);

        idx = findAccountByName(name);
        if (idx == -1) {
            printf("Account not found.\n");
        } else {
            printf("Current Balance: %.2f\n", g_accounts[idx].currentBalance);
            printf("Currency: PHP\n\n");
            printf("Deposit Amount: ");
            if (scanf("%lf", &deposit) == 1 && deposit > 0.0) {
                double newBal = g_accounts[idx].currentBalance + deposit;
                appendTransactionToCSV(g_accounts[idx].accountName, g_accounts[idx].accountNumber,
                                       "deposit", deposit, newBal);
                snprintf(g_activeAccountName, sizeof(g_activeAccountName), "%s", g_accounts[idx].accountName);
                g_activeBalance = newBal;
                printf("Updated Balance: %.2f\n", newBal);
            } else {
                clearInputBuffer();
                printf("Invalid deposit amount.\n");
            }
        }

        printf("\nDeposit another amount? (Y/N): ");
        scanf(" %c", &another);
    } while (another == 'Y' || another == 'y');
}

/* Menu [3]: Withdraw Amount */
void handleWithdrawAmount(void) {
    char another;
    if (!g_dataLoaded) {
        loadTransactionsFromCSV(0);
    }

    do {
        char name[64];
        double withdraw = 0.0;
        int idx;

        printf("\nWithdraw Amount\n");
        printf("Account Name: ");
        scanf(" %63[^\n]", name);

        idx = findAccountByName(name);
        if (idx == -1) {
            printf("Account not found.\n");
        } else {
            printf("Current Balance: %.2f\n", g_accounts[idx].currentBalance);
            printf("Currency: PHP\n\n");
            printf("Withdraw Amount: ");
            if (scanf("%lf", &withdraw) == 1 && withdraw > 0.0 && withdraw <= g_accounts[idx].currentBalance) {
                double newBal = g_accounts[idx].currentBalance - withdraw;
                appendTransactionToCSV(g_accounts[idx].accountName, g_accounts[idx].accountNumber,
                                       "withdrawal", withdraw, newBal);
                snprintf(g_activeAccountName, sizeof(g_activeAccountName), "%s", g_accounts[idx].accountName);
                g_activeBalance = newBal;
                printf("Updated Balance: %.2f\n", newBal);
            } else {
                clearInputBuffer();
                printf("Invalid withdrawal amount or insufficient balance.\n");
            }
        }

        printf("\nWithdraw another amount? (Y/N): ");
        scanf(" %c", &another);
    } while (another == 'Y' || another == 'y');
}

/* Menu [4]: Currency Exchange */
void handleCurrencyExchange(void) {
    char convertAnother;
    do {
        int srcCurrency = 0, destCurrency = 0;
        double srcAmount = 0.0, destAmount = 0.0;

        printf("\nForeign Currency Exchange\n");
        printf("Source Currency Option:\n");
        printCurrencyOptions();

        printf("\nSource Currency: ");
        if (scanf("%d", &srcCurrency) != 1 || srcCurrency < 1 || srcCurrency > 6) {
            clearInputBuffer();
            break;
        }

        printf("Source Amount: ");
        if (scanf("%lf", &srcAmount) != 1 || srcAmount < 0.0) {
            clearInputBuffer();
            break;
        }

        printf("\nExchanged Currency Options:\n");
        printCurrencyOptions();

        printf("\nExchange Currency: ");
        if (scanf("%d", &destCurrency) != 1 || destCurrency < 1 || destCurrency > 6) {
            clearInputBuffer();
            break;
        }

        destAmount = (srcAmount * g_rates[srcCurrency]) / g_rates[destCurrency];
        printf("Exchange Amount: %.2f\n", destAmount);

        printf("\nConvert another currency (Y/N)? ");
        scanf(" %c", &convertAnother);
    } while (convertAnother == 'Y' || convertAnother == 'y');
}

/* Menu [5]: Record Exchange Rates */
void handleRecordExchangeRates(void) {
    int currencyIndex = 0;
    double newRate = 0.0;

    printf("\nRecord Exchange Rate\n");
    printCurrencyOptions();

    printf("\nSelect Foreign Currency: ");
    if (scanf("%d", &currencyIndex) != 1 || currencyIndex < 2 || currencyIndex > 6) {
        clearInputBuffer();
        return;
    }

    printf("Exchange Rate: ");
    if (scanf("%lf", &newRate) == 1 && newRate > 0.0) {
        g_rates[currencyIndex] = newRate;
    } else {
        clearInputBuffer();
    }
}

/* Menu [6]: Show Interest Amount */
void handleShowInterestAmount(void) {
    char name[64];
    int days = 0;
    int i, idx;
    double tempBalance = g_activeBalance;

    if (!g_dataLoaded) {
        loadTransactionsFromCSV(0);
    }

    printf("\nShow Interest Amount\n");
    printf("Account Name: ");
    scanf(" %63[^\n]", name);

    idx = findAccountByName(name);
    if (idx != -1) {
        tempBalance = g_accounts[idx].currentBalance;
        snprintf(g_activeAccountName, sizeof(g_activeAccountName), "%s", g_accounts[idx].accountName);
        g_activeBalance = tempBalance;
    }

    printf("Current Balance: %.2f\n", tempBalance);
    printf("Currency: PHP\n");
    printf("Interest Rate: 5%%\n");
    printf("Total Number of Days: ");

    if (scanf("%d", &days) == 1 && days > 0) {
        clearInputBuffer();
        printf("\nDay\t| Interest\t| Balance\t|\n");
        for (i = 1; i <= days; i++) {
            double dailyInterest = tempBalance * (0.05 / 365.0);
            double roundedInterest = ((long long)(dailyInterest * 100.0 + 0.5)) / 100.0;
            tempBalance += roundedInterest;
            printf("%d\t| %.2f\t\t| %.2f\t|\n", i, roundedInterest, tempBalance);
        }
    } else {
        clearInputBuffer();
    }
}

/* Comparator for Report 1: descending by AverageDailyBalance. */
int cmpReport1(const void *a, const void *b) {
    const AccountSummary *x = (const AccountSummary *)a;
    const AccountSummary *y = (const AccountSummary *)b;
    if (x->avgDailyBalance < y->avgDailyBalance) return 1;
    if (x->avgDailyBalance > y->avgDailyBalance) return -1;
    return strcmp(x->accountName, y->accountName);
}

/* Comparator for Report 2: descending by ExpectedInterest. */
int cmpReport2(const void *a, const void *b) {
    const AccountSummary *x = (const AccountSummary *)a;
    const AccountSummary *y = (const AccountSummary *)b;
    if (x->expectedInterest < y->expectedInterest) return 1;
    if (x->expectedInterest > y->expectedInterest) return -1;
    return strcmp(x->accountName, y->accountName);
}

/* Comparator for Report 3: ascending by Date, descending by RiskFlagLevel. */
int cmpReport3(const void *a, const void *b) {
    const DailyBehavior *x = (const DailyBehavior *)a;
    const DailyBehavior *y = (const DailyBehavior *)b;
    int dateCmp = compareDates(x->day, x->month, x->year, y->day, y->month, y->year);
    int riskCmp;
    if (dateCmp != 0) {
        return dateCmp;
    }
    riskCmp = strcmp(y->riskFlag, x->riskFlag);
    if (riskCmp != 0) {
        return riskCmp;
    }
    return strcmp(x->accountName, y->accountName);
}

/* Comparator for sorting transactions chronologically for daily grouping. */
int cmpTxnByAccDate(const void *a, const void *b) {
    const Transaction *x = (const Transaction *)a;
    const Transaction *y = (const Transaction *)b;
    int nameCmp = strcmp(x->accountName, y->accountName);
    int dateCmp;
    if (nameCmp != 0) {
        return nameCmp;
    }
    dateCmp = compareDates(x->day, x->month, x->year, y->day, y->month, y->year);
    if (dateCmp != 0) {
        return dateCmp;
    }
    return (x->hour * 3600 + x->minute * 60 + x->second) -
           (y->hour * 3600 + y->minute * 60 + y->second);
}

/* Generates Reports 1, 2, 3 and summary.json. */
void generateAllReports(void) {
    int i, j, d;
    int maxDay = 0, maxMonth = 0, maxYear = 0;
    long maxJDN;
    int targetDaysInMonth;
    double totalDeposits = 0.0;
    double totalWithdrawals = 0.0;
    double totalBalance = 0.0;
    double totalInterestExpense = 0.0;
    FILE *fp;
    static Transaction sortedTxns[MAX_TXNS];

    if (!g_dataLoaded || g_txnCount == 0) {
        printf("Please load the dataset first.\n");
        return;
    }

    printf("Generating reports...\n");

    for (i = 0; i < g_txnCount; i++) {
        if (compareDates(g_txns[i].day, g_txns[i].month, g_txns[i].year, maxDay, maxMonth, maxYear) > 0) {
            maxDay = g_txns[i].day;
            maxMonth = g_txns[i].month;
            maxYear = g_txns[i].year;
        }
        if (strcmp(g_txns[i].type, "deposit") == 0) {
            totalDeposits += g_txns[i].amount;
        } else if (strcmp(g_txns[i].type, "withdrawal") == 0) {
            totalWithdrawals += g_txns[i].amount;
        }
    }

    maxJDN = dateToJDN(maxDay, maxMonth, maxYear);
    targetDaysInMonth = daysInMonth(maxMonth, maxYear);

    memcpy(sortedTxns, g_txns, (size_t)g_txnCount * sizeof(Transaction));
    qsort(sortedTxns, (size_t)g_txnCount, sizeof(Transaction), cmpTxnByAccDate);

    /* Report 1: Banking Privilege Tiers */
    g_report1Count = 0;
    for (i = 0; i < g_accountCount; i++) {
        AccountSummary acc = g_accounts[i];
        double carryBalance = 0.0;
        double eodBalance[32];
        int hasTxnOnDay[32];
        double sumEOD = 0.0;

        for (d = 1; d <= 31; d++) {
            eodBalance[d] = 0.0;
            hasTxnOnDay[d] = 0;
        }

        for (j = 0; j < g_txnCount; j++) {
            if (strcmp(sortedTxns[j].accountName, acc.accountName) != 0) {
                continue;
            }
            if (compareDates(sortedTxns[j].day, sortedTxns[j].month, sortedTxns[j].year, 1, maxMonth, maxYear) < 0) {
                carryBalance = sortedTxns[j].currentBalance;
            } else if (sortedTxns[j].month == maxMonth && sortedTxns[j].year == maxYear) {
                eodBalance[sortedTxns[j].day] = sortedTxns[j].currentBalance;
                hasTxnOnDay[sortedTxns[j].day] = 1;
            }
        }

        for (d = 1; d <= targetDaysInMonth; d++) {
            if (hasTxnOnDay[d]) {
                carryBalance = eodBalance[d];
            }
            sumEOD += carryBalance;
        }

        acc.avgDailyBalance = sumEOD / (double)targetDaysInMonth;
        acc.accountAge = (int)(maxJDN - dateToJDN(acc.firstDay, acc.firstMonth, acc.firstYear));

        if (acc.avgDailyBalance >= 100000.00) {
            if (acc.avgDailyBalance < 500000.00) {
                snprintf(acc.privilegeTier, sizeof(acc.privilegeTier), "Silver");
            } else if (acc.avgDailyBalance < 1000000.00) {
                snprintf(acc.privilegeTier, sizeof(acc.privilegeTier), "Gold");
            } else {
                snprintf(acc.privilegeTier, sizeof(acc.privilegeTier), "Platinum");
            }
            g_report1[g_report1Count++] = acc;
        }
    }
    qsort(g_report1, (size_t)g_report1Count, sizeof(AccountSummary), cmpReport1);

    fp = fopen("report1_banking_privilege_tiers.csv", "w");
    if (fp != NULL) {
        fprintf(fp, "AccountName,AccountAge,CurrentBalance,AverageDailyBalance,PrivilegeTier\n");
        for (i = 0; i < g_report1Count; i++) {
            char balBuf[32], adbBuf[32];
            formatCommaNumber(g_report1[i].currentBalance, balBuf, sizeof(balBuf));
            formatCommaNumber(g_report1[i].avgDailyBalance, adbBuf, sizeof(adbBuf));
            fprintf(fp, "%s,%d,\"%s\",\"%s\",%s\n",
                    g_report1[i].accountName, g_report1[i].accountAge, balBuf, adbBuf, g_report1[i].privilegeTier);
        }
        fclose(fp);
    }

    /* Report 2: Interest Expense Projection */
    g_report2Count = 0;
    for (i = 0; i < g_accountCount; i++) {
        AccountSummary acc = g_accounts[i];
        totalBalance += acc.currentBalance;

        if (acc.currentBalance >= 10000.00) {
            double tempBal = acc.currentBalance;
            double totalInt = 0.0;
            for (d = 1; d <= 30; d++) {
                double dailyInt = tempBal * (0.05 / 365.0);
                tempBal += dailyInt;
                totalInt += dailyInt;
            }
            acc.expectedInterest = totalInt;
            totalInterestExpense += totalInt;
            g_report2[g_report2Count++] = acc;
        }
    }
    qsort(g_report2, (size_t)g_report2Count, sizeof(AccountSummary), cmpReport2);

    fp = fopen("report2_interest_expense_projection.csv", "w");
    if (fp != NULL) {
        fprintf(fp, "AccountName,CurrentBalance,ExpectedInterest\n");
        for (i = 0; i < g_report2Count; i++) {
            char balBuf[32], intBuf[32];
            formatCommaNumber(g_report2[i].currentBalance, balBuf, sizeof(balBuf));
            formatCommaNumber(g_report2[i].expectedInterest, intBuf, sizeof(intBuf));
            fprintf(fp, "%s,\"%s\",\"%s\"\n",
                    g_report2[i].accountName, balBuf, intBuf);
        }
        fclose(fp);
    }

    /* Report 3: Suspicious Client Behavior */
    g_report3Count = 0;
    i = 0;
    while (i < g_txnCount && g_report3Count < MAX_DAILY_RECS) {
        DailyBehavior rec;
        snprintf(rec.accountName, sizeof(rec.accountName), "%s", sortedTxns[i].accountName);
        rec.day = sortedTxns[i].day;
        rec.month = sortedTxns[i].month;
        rec.year = sortedTxns[i].year;
        rec.totalWithdrawals = 0.0;
        rec.numDeposits = 0;
        rec.totalDeposits = 0.0;

        while (i < g_txnCount &&
               strcmp(sortedTxns[i].accountName, rec.accountName) == 0 &&
               sortedTxns[i].day == rec.day &&
               sortedTxns[i].month == rec.month &&
               sortedTxns[i].year == rec.year) {
            if (strcmp(sortedTxns[i].type, "withdrawal") == 0) {
                rec.totalWithdrawals += sortedTxns[i].amount;
            } else if (strcmp(sortedTxns[i].type, "deposit") == 0) {
                rec.numDeposits++;
                rec.totalDeposits += sortedTxns[i].amount;
            }
            i++;
        }

        if (rec.totalDeposits > 0.0) {
            rec.turnoverVelocity = (rec.totalWithdrawals / rec.totalDeposits) * 100.0;
        } else {
            rec.turnoverVelocity = 0.0;
        }

        if (rec.totalWithdrawals >= 500000.00 || rec.turnoverVelocity >= 90.0) {
            snprintf(rec.riskFlag, sizeof(rec.riskFlag), "Suspicious");
        } else {
            snprintf(rec.riskFlag, sizeof(rec.riskFlag), "Normal");
        }

        g_report3[g_report3Count++] = rec;
    }
    qsort(g_report3, (size_t)g_report3Count, sizeof(DailyBehavior), cmpReport3);

    fp = fopen("report3_suspicious_client_behavior.csv", "w");
    if (fp != NULL) {
        fprintf(fp, "AccountName,Date,TotalWithdrawalAmounts,NumberOfDeposits,TotalDepositAmounts,TurnoverVelocity,RiskFlagLevel\n");
        for (i = 0; i < g_report3Count; i++) {
            char wBuf[32], dBuf[32], vBuf[32];
            formatCommaNumber(g_report3[i].totalWithdrawals, wBuf, sizeof(wBuf));
            formatCommaNumber(g_report3[i].totalDeposits, dBuf, sizeof(dBuf));
            formatCommaNumber(g_report3[i].turnoverVelocity, vBuf, sizeof(vBuf));
            fprintf(fp, "%s,%02d/%02d/%04d,\"%s\",%d,\"%s\",\"%s\",%s\n",
                    g_report3[i].accountName, g_report3[i].day, g_report3[i].month, g_report3[i].year,
                    wBuf, g_report3[i].numDeposits, dBuf, vBuf, g_report3[i].riskFlag);
        }
        fclose(fp);
    }

    /* Summary JSON */
    fp = fopen("summary.json", "w");
    if (fp != NULL) {
        fprintf(fp, "{\"total_accounts\": %d, \"total_deposits\": %.2f, \"total_withdrawals\": %.2f, \"total_balance\": %.2f, \"total_interest_expense\": %.2f}\n",
                g_accountCount, totalDeposits, totalWithdrawals, totalBalance, totalInterestExpense);
        fclose(fp);
    }

    printf("Outputs saved to individual files...\n\n");

    printf("Report 1: Banking Privilege Tiers\n");
    printf("%-22s | %-10s | %-15s | %-19s | %-10s\n",
           "AccountName", "AccountAge", "CurrentBalance", "AverageDailyBalance", "Tier");
    for (i = 0; i < g_report1Count && i < 2; i++) {
        char balBuf[32], adbBuf[32];
        formatCommaNumber(g_report1[i].currentBalance, balBuf, sizeof(balBuf));
        formatCommaNumber(g_report1[i].avgDailyBalance, adbBuf, sizeof(adbBuf));
        printf("%-22s | %-10d | %-15s | %-19s | %-10s\n",
               g_report1[i].accountName, g_report1[i].accountAge, balBuf, adbBuf, g_report1[i].privilegeTier);
    }
    printf("(Full table exported to report1_banking_privilege_tiers.csv)\n\n");

    printf("Report 2: Interest Expense Projection\n");
    printf("%-22s | %-15s | %-16s\n", "AccountName", "CurrentBalance", "ExpectedInterest");
    for (i = 0; i < g_report2Count && i < 2; i++) {
        char balBuf[32], intBuf[32];
        formatCommaNumber(g_report2[i].currentBalance, balBuf, sizeof(balBuf));
        formatCommaNumber(g_report2[i].expectedInterest, intBuf, sizeof(intBuf));
        printf("%-22s | %-15s | %-16s\n",
               g_report2[i].accountName, balBuf, intBuf);
    }
    printf("(Full table exported to report2_interest_expense_projection.csv)\n\n");

    printf("Report 3: Suspicious Client Behavior\n");
    printf("%-20s | %-10s | %-14s | %-8s | %-14s | %-10s | %-10s\n",
           "AccountName", "Date", "Withdrawals", "Deposits", "DepositAmt", "Velocity%", "RiskFlag");
    for (i = 0; i < g_report3Count && i < 2; i++) {
        char wBuf[32], dBuf[32], vBuf[32];
        formatCommaNumber(g_report3[i].totalWithdrawals, wBuf, sizeof(wBuf));
        formatCommaNumber(g_report3[i].totalDeposits, dBuf, sizeof(dBuf));
        formatCommaNumber(g_report3[i].turnoverVelocity, vBuf, sizeof(vBuf));
        printf("%-20s | %02d/%02d/%04d | %-14s | %-8d | %-14s | %-10s | %-10s\n",
               g_report3[i].accountName, g_report3[i].day, g_report3[i].month, g_report3[i].year,
               wBuf, g_report3[i].numDeposits, dBuf, vBuf, g_report3[i].riskFlag);
    }
    printf("(Full table exported to report3_suspicious_client_behavior.csv)\n\n");

    printf("Summary State (summary.json):\n");
    printf("{\"total_accounts\": %d, \"total_deposits\": %.2f, \"total_withdrawals\": %.2f, \"total_balance\": %.2f, \"total_interest_expense\": %.2f}\n",
           g_accountCount, totalDeposits, totalWithdrawals, totalBalance, totalInterestExpense);
}

/* Menu [7]: Generate Reports */
void handleGenerateReports(void) {
    int subChoice = 0;
    printf("\nGenerate Reports\n");
    printf("[1] Load the file\n");
    printf("[2] Generate Reports\n");
    printf("Enter choice: ");

    if (scanf("%d", &subChoice) != 1) {
        clearInputBuffer();
        return;
    }

    if (subChoice == 1) {
        loadTransactionsFromCSV(1);
    } else if (subChoice == 2) {
        generateAllReports();
    } else {
        printf("Invalid choice.\n");
    }
}

int main(void) {
    int choice;
    char returnToMain;

    do {
        printf("\nSelect Transaction:\n");
        printf("[1] Register Account\n");
        printf("[2] Deposit Amount\n");
        printf("[3] Withdraw Amount\n");
        printf("[4] Currency Exchange\n");
        printf("[5] Record Exchange Rates\n");
        printf("[6] Show Interest Amount\n");
        printf("[7] Generate Reports\n");

        printf("\nChoice: ");
        if (scanf("%d", &choice) != 1) {
            clearInputBuffer();
            choice = 0;
        }

        switch (choice) {
            case 1:
                handleRegisterAccount();
                break;
            case 2:
                handleDepositAmount();
                break;
            case 3:
                handleWithdrawAmount();
                break;
            case 4:
                handleCurrencyExchange();
                break;
            case 5:
                handleRecordExchangeRates();
                break;
            case 6:
                handleShowInterestAmount();
                break;
            case 7:
                handleGenerateReports();
                break;
            default:
                break;
        }

        printf("\nBack to the Main Menu (Y/N): ");
        scanf(" %c", &returnToMain);
    } while (returnToMain == 'Y' || returnToMain == 'y');

    return 0;
}
