/*
Last Names: Acosta, Cagigas, Pangilinan, Po
Language: C
Paradigm(s): Procedural Programming

BUILD : gcc -Wall -Wextra -o bank MP_8_C.c
RUN   : ./bank            (bank_transactions.csv must be in the working directory)

ASSUMPTIONS
 - "Last month" (Report 1) is the calendar month of the latest transaction in
   the dataset; AccountAge is measured up to the latest transaction date.
 - Report 2 compounds the interest daily for 30 days; each day's interest is
   rounded to centavos. Menu [6] uses the same routine, so both always agree.
 - Report 3: TurnoverVelocity is 0 on days with no deposits (division by zero
   is undefined); such days are Suspicious only if withdrawals >= Php 500,000.
 - Account names are matched case-insensitively. "Last, First" is accepted
   as input and converted to "First Last".
 - Report 4 (custom report) is deferred to MCO3.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#define CSV_FILENAME "bank_transactions.csv"
#define CSV_HEADER "account_name,account_number,date,time,currency,amount,type,current_balance"
#define MAX_TXNS 20000
#define MAX_ACCOUNTS 2000
#define MAX_DAILY_RECS 20000
#define MAX_LINE 512
#define MAX_NAME 64
#define MIN_INITIAL_DEPOSIT 5000.00
#define MAX_AMOUNT 1.0e12
#define ANNUAL_RATE 0.05

typedef struct {
    char accountName[MAX_NAME];
    long long accountNumber;
    int day;
    int month;
    int year;
    int hour;
    int minute;
    int second;
    int seq;               /* original load order; breaks ties for equal timestamps */
    char currency[16];
    double amount;
    char type[16];
    double currentBalance;
} Transaction;

typedef struct {
    char accountName[MAX_NAME];
    long long accountNumber;
    int firstDay;
    int firstMonth;
    int firstYear;
    int lastDay;
    int lastMonth;
    int lastYear;
    int lastSecOfDay;
    int lastSeq;
    double currentBalance;
    double avgDailyBalance;
    double expectedInterest;
    int accountAge;
    char privilegeTier[16];
} AccountSummary;

typedef struct {
    char accountName[MAX_NAME];
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
static int g_seqCounter = 0;
static int g_totalRowsRead = 0;
static int g_invalidRows = 0;
static int g_droppedRows = 0;
static int g_dataLoaded = 0;
static int g_inputEnded = 0;
static long long g_maxAccountNumber = 1092000000LL;

static AccountSummary g_accounts[MAX_ACCOUNTS];
static int g_accountCount = 0;

static AccountSummary g_report1[MAX_ACCOUNTS];
static int g_report1Count = 0;

static AccountSummary g_report2[MAX_ACCOUNTS];
static int g_report2Count = 0;

static DailyBehavior g_report3[MAX_DAILY_RECS];
static int g_report3Count = 0;

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

/* ------------------------------------------------------------------ */
/* Input helpers (line based, EOF safe)                               */
/* ------------------------------------------------------------------ */

#define IN_OK 1
#define IN_INVALID 0
#define IN_EOF (-1)
#define IN_BLANK 2

/* Reads one line from stdin without the newline; discards any overflow. Returns 0 on EOF. */
int readLine(char *buf, size_t size) {
    size_t len;
    if (fgets(buf, (int)size, stdin) == NULL) {
        g_inputEnded = 1;
        buf[0] = '\0';
        return 0;
    }
    len = strlen(buf);
    if (len > 0 && buf[len - 1] == '\n') {
        buf[--len] = '\0';
    } else {
        int c;
        while ((c = getchar()) != '\n' && c != EOF) {
        }
    }
    if (len > 0 && buf[len - 1] == '\r') {
        buf[len - 1] = '\0';
    }
    return 1;
}

/* Returns 1 if the string has only whitespace. */
int isBlank(const char *s) {
    while (*s) {
        if (!isspace((unsigned char)*s)) return 0;
        s++;
    }
    return 1;
}

/* Prompts for a number. Commas are allowed (e.g. 10,000.00). */
int promptDouble(const char *prompt, double *out) {
    char line[128], clean[128], *endPtr;
    size_t i, j = 0;
    printf("%s", prompt);
    if (!readLine(line, sizeof(line))) return IN_EOF;
    if (isBlank(line)) return IN_BLANK;
    for (i = 0; line[i] != '\0' && j < sizeof(clean) - 1; i++) {
        if (line[i] != ',') clean[j++] = line[i];
    }
    clean[j] = '\0';
    *out = strtod(clean, &endPtr);
    while (*endPtr && isspace((unsigned char)*endPtr)) endPtr++;
    if (endPtr == clean || *endPtr != '\0') return IN_INVALID;
    if (!(*out > -MAX_AMOUNT && *out < MAX_AMOUNT)) return IN_INVALID; /* rejects nan/inf */
    return IN_OK;
}

/* Prompts for an integer. */
int promptInt(const char *prompt, int *out) {
    char line[64], *endPtr;
    long v;
    printf("%s", prompt);
    if (!readLine(line, sizeof(line))) return IN_EOF;
    if (isBlank(line)) return IN_BLANK;
    v = strtol(line, &endPtr, 10);
    while (*endPtr && isspace((unsigned char)*endPtr)) endPtr++;
    if (endPtr == line || *endPtr != '\0' || v < -1000000L || v > 1000000L) return IN_INVALID;
    *out = (int)v;
    return IN_OK;
}

/* Asks a Y/N question until answered. EOF counts as N. */
int promptYesNo(const char *prompt) {
    char line[32];
    char *p;
    for (;;) {
        printf("%s", prompt);
        if (!readLine(line, sizeof(line))) return 0;
        p = line;
        while (*p && isspace((unsigned char)*p)) p++;
        if (*p == 'Y' || *p == 'y') return 1;
        if (*p == 'N' || *p == 'n') return 0;
        printf("Please enter Y or N.\n");
    }
}

/* Rounds a monetary value to the nearest centavo. */
double roundCents(double v) {
    return (v < 0.0) ? -(double)((long long)(-v * 100.0 + 0.5)) / 100.0
                     : (double)((long long)(v * 100.0 + 0.5)) / 100.0;
}

/* ------------------------------------------------------------------ */
/* Name helpers                                                       */
/* ------------------------------------------------------------------ */

/* Case-insensitive name comparison. */
int compareNames(const char *a, const char *b) {
    while (*a && *b) {
        int ca = tolower((unsigned char)*a);
        int cb = tolower((unsigned char)*b);
        if (ca != cb) return ca - cb;
        a++;
        b++;
    }
    return tolower((unsigned char)*a) - tolower((unsigned char)*b);
}

/* Trims, collapses spaces, and converts "Last, First" into "First Last". */
void normalizeName(char *name) {
    char tmp[128];
    size_t i, j = 0;
    int pendingSpace = 0;
    char *comma;

    for (i = 0; name[i] != '\0' && j < sizeof(tmp) - 1; i++) {
        if (isspace((unsigned char)name[i])) {
            pendingSpace = (j > 0);
        } else {
            if (pendingSpace) {
                tmp[j++] = ' ';
                pendingSpace = 0;
            }
            if (j < sizeof(tmp) - 1) tmp[j++] = name[i];
        }
    }
    tmp[j] = '\0';

    comma = strchr(tmp, ',');
    if (comma != NULL && strchr(comma + 1, ',') == NULL) {
        char left[128], right[128];
        char *r = comma + 1;
        size_t l = (size_t)(comma - tmp);
        while (l > 0 && tmp[l - 1] == ' ') l--;
        memcpy(left, tmp, l);
        left[l] = '\0';
        while (*r == ' ') r++;
        snprintf(right, sizeof(right), "%s", r);
        if (left[0] != '\0' && right[0] != '\0' && strlen(left) + strlen(right) + 2 <= sizeof(tmp)) {
            size_t rl = strlen(right), ll = strlen(left);
            memcpy(tmp, right, rl);
            tmp[rl] = ' ';
            memcpy(tmp + rl + 1, left, ll);
            tmp[rl + 1 + ll] = '\0';
        }
    }
    snprintf(name, 128, "%s", tmp);
}

/* A valid new account name has 2+ words made of letters, hyphens, apostrophes or periods. */
int isValidFullName(const char *name) {
    int words = 0, inWord = 0;
    size_t i, len = strlen(name);
    if (len == 0 || len >= MAX_NAME) return 0;
    for (i = 0; i < len; i++) {
        unsigned char c = (unsigned char)name[i];
        if (c == ' ') {
            inWord = 0;
        } else if (isalpha(c) || c == '-' || c == '\'' || c == '.') {
            if (!inWord) words++;
            inWord = 1;
        } else {
            return 0;
        }
    }
    return words >= 2;
}

/* Prompts for an account name and normalizes it. Returns IN_OK / IN_BLANK / IN_EOF. */
int promptName(const char *prompt, char *name, size_t size) {
    char line[128];
    printf("%s", prompt);
    if (!readLine(line, sizeof(line))) return IN_EOF;
    normalizeName(line);
    if (line[0] == '\0') return IN_BLANK;
    snprintf(name, size, "%s", line);
    return IN_OK;
}

/* ------------------------------------------------------------------ */
/* Formatting / date helpers                                          */
/* ------------------------------------------------------------------ */

/* Prints the 6 supported currency options. */
void printCurrencyOptions(void) {
    int i;
    for (i = 1; i <= 6; i++) {
        printf("[%d] %s\n", i, g_currencyNames[i]);
    }
}

/* Formats a double with comma separators and the given number of decimals. */
static void formatWithCommas(double value, int decimals, char *outBuf, size_t bufSize) {
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

    snprintf(raw, sizeof(raw), "%.*f", decimals, value);
    dot = strchr(raw, '.');
    if (dot != NULL) {
        size_t intLen = (size_t)(dot - raw);
        memcpy(intPart, raw, intLen);
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

/* e.g. 1234567.891 -> "1,234,567.89" */
void formatCommaNumber(double value, char *outBuf, size_t bufSize) {
    formatWithCommas(value, 2, outBuf, bufSize);
}

/* e.g. 9191 -> "9,191" */
void formatCommaInt(long long value, char *outBuf, size_t bufSize) {
    formatWithCommas((double)value, 0, outBuf, bufSize);
}

/* Writes a CSV text field, quoting it if it contains a comma or quote. */
void writeCsvText(FILE *fp, const char *s) {
    if (strpbrk(s, ",\"") != NULL) {
        const char *p;
        fputc('"', fp);
        for (p = s; *p; p++) {
            if (*p == '"') fputc('"', fp);
            fputc(*p, fp);
        }
        fputc('"', fp);
    } else {
        fputs(s, fp);
    }
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

/* ------------------------------------------------------------------ */
/* Interest                                                           */
/* ------------------------------------------------------------------ */

/* Daily interest = balance x (annual rate / 365), rounded to centavos. */
double dailyInterest(double balance) {
    return roundCents(balance * (ANNUAL_RATE / 365.0));
}

/* Total compounded interest over `days` days (shared by Menu [6] and Report 2). */
double projectInterest(double balance, int days) {
    double total = 0.0;
    int i;
    for (i = 0; i < days; i++) {
        double di = dailyInterest(balance);
        balance += di;
        total += di;
    }
    return total;
}

/* ------------------------------------------------------------------ */
/* Accounts and CSV                                                   */
/* ------------------------------------------------------------------ */

/* Finds an account index by name (case-insensitive), or -1 if not found. */
int findAccountByName(const char *name) {
    int i;
    for (i = 0; i < g_accountCount; i++) {
        if (compareNames(g_accounts[i].accountName, name) == 0) {
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
            g_accounts[idx].lastSeq = t->seq;
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
            if (cmpLast > 0 ||
                (cmpLast == 0 && (sec > g_accounts[idx].lastSecOfDay ||
                                  (sec == g_accounts[idx].lastSecOfDay && t->seq > g_accounts[idx].lastSeq)))) {
                g_accounts[idx].lastDay = t->day;
                g_accounts[idx].lastMonth = t->month;
                g_accounts[idx].lastYear = t->year;
                g_accounts[idx].lastSecOfDay = sec;
                g_accounts[idx].lastSeq = t->seq;
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
    g_seqCounter = 0;
    g_totalRowsRead = 0;
    g_invalidRows = 0;
    g_droppedRows = 0;

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
        } else if (!feof(fp)) {
            /* line longer than buffer: count as one invalid row and skip the rest */
            int c;
            while ((c = fgetc(fp)) != '\n' && c != EOF) {
            }
            g_totalRowsRead++;
            g_invalidRows++;
            continue;
        }
        if (isBlank(line)) {
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

        /* exactly 8 fields: a leftover comma means a 9th field */
        if (fieldIdx < 8 || (p != NULL && *p != '\0') || strchr(fields[7], ',') != NULL) {
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
        if (*endPtr != '\0' || accNum <= 0) {
            g_invalidRows++;
            continue;
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
        if (*endPtr != '\0' || !(amt >= 0.0 && amt < MAX_AMOUNT)) {
            g_invalidRows++;
            continue;
        }

        bal = strtod(fields[7], &endPtr);
        if (*endPtr != '\0' || !(bal > -MAX_AMOUNT && bal < MAX_AMOUNT)) {
            g_invalidRows++;
            continue;
        }

        if (strcmp(fields[6], "deposit") != 0 && strcmp(fields[6], "withdrawal") != 0) {
            g_invalidRows++;
            continue;
        }

        /* the row is structurally valid, so its account number counts as used */
        if (accNum > g_maxAccountNumber) {
            g_maxAccountNumber = accNum;
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
            t->seq = g_seqCounter++;
            snprintf(t->currency, sizeof(t->currency), "%s", fields[4]);
            t->amount = amt;
            snprintf(t->type, sizeof(t->type), "%s", fields[6]);
            t->currentBalance = bal;
            phpCount++;
        } else {
            g_droppedRows++;
        }
    }

    fclose(fp);
    g_dataLoaded = 1;
    rebuildAccountSummaries();

    if (verbose) {
        char totalBuf[32], phpBuf[32];
        formatCommaInt(g_totalRowsRead, totalBuf, sizeof(totalBuf));
        formatCommaInt(phpCount, phpBuf, sizeof(phpBuf));
        printf("Processing dataset... (%s rows loaded, %s filtered for PHP currency", totalBuf, phpBuf);
        if (g_invalidRows > 0) {
            printf(", %d invalid rows skipped", g_invalidRows);
        }
        printf(")\n");
        if (g_droppedRows > 0) {
            printf("Warning: %d rows exceeded the %d-transaction capacity and were not loaded.\n",
                   g_droppedRows, MAX_TXNS);
        }
    }

    return 1;
}

/* Makes sure the CSV exists, has a header, and ends with a CRLF before appending. */
static int prepareCsvForAppend(void) {
    FILE *fp = fopen(CSV_FILENAME, "rb");
    long size = 0;
    int last = '\n';

    if (fp != NULL) {
        fseek(fp, 0, SEEK_END);
        size = ftell(fp);
        if (size > 0) {
            fseek(fp, -1, SEEK_END);
            last = fgetc(fp);
        }
        fclose(fp);
    }

    fp = fopen(CSV_FILENAME, "ab");
    if (fp == NULL) {
        return 0;
    }
    if (size <= 0) {
        fputs(CSV_HEADER "\r\n", fp);
    } else if (last != '\n') {
        fputs("\r\n", fp);
    }
    fclose(fp);
    return 1;
}

/* Appends a single transaction row to bank_transactions.csv. */
int appendTransactionToCSV(const char *accName, long long accNum, const char *type, double amount, double newBalance) {
    FILE *fp;
    time_t now = time(NULL);
    struct tm *t = localtime(&now);
    char dateStr[32];
    char timeStr[32];
    struct { int day, month, year, sec; } stamp;
    int idx;

    if (t == NULL || !prepareCsvForAppend() || (fp = fopen(CSV_FILENAME, "ab")) == NULL) {
        printf("Error: Could not write to %s.\n", CSV_FILENAME);
        return 0;
    }

    stamp.day = t->tm_mday;
    stamp.month = t->tm_mon + 1;
    stamp.year = t->tm_year + 1900;
    stamp.sec = t->tm_hour * 3600 + t->tm_min * 60 + t->tm_sec;

    /* The dataset may contain dates later than the system clock. Never stamp a new
       row earlier than the account's latest transaction, or the balance would go stale. */
    idx = findAccountByName(accName);
    if (idx != -1) {
        int c = compareDates(g_accounts[idx].lastDay, g_accounts[idx].lastMonth, g_accounts[idx].lastYear,
                             stamp.day, stamp.month, stamp.year);
        if (c > 0 || (c == 0 && g_accounts[idx].lastSecOfDay > stamp.sec)) {
            stamp.day = g_accounts[idx].lastDay;
            stamp.month = g_accounts[idx].lastMonth;
            stamp.year = g_accounts[idx].lastYear;
            stamp.sec = g_accounts[idx].lastSecOfDay;
        }
    }

    snprintf(dateStr, sizeof(dateStr), "%02d/%02d/%04d", stamp.day, stamp.month, stamp.year);
    snprintf(timeStr, sizeof(timeStr), "%02d:%02d:%02d", stamp.sec / 3600, (stamp.sec / 60) % 60, stamp.sec % 60);

    writeCsvText(fp, accName);
    fprintf(fp, ",%lld,%s,%s,PHP,%.2f,%s,%.2f\r\n", accNum, dateStr, timeStr, amount, type, newBalance);
    if (fclose(fp) != 0) {
        printf("Error: Could not write to %s.\n", CSV_FILENAME);
        return 0;
    }

    if (g_dataLoaded) {
        Transaction *tx = &g_txns[g_txnCount++];
        snprintf(tx->accountName, sizeof(tx->accountName), "%s", accName);
        tx->accountNumber = accNum;
        tx->day = stamp.day;
        tx->month = stamp.month;
        tx->year = stamp.year;
        tx->hour = stamp.sec / 3600;
        tx->minute = (stamp.sec / 60) % 60;
        tx->second = stamp.sec % 60;
        tx->seq = g_seqCounter++;
        snprintf(tx->currency, sizeof(tx->currency), "PHP");
        tx->amount = amount;
        snprintf(tx->type, sizeof(tx->type), "%s", type);
        tx->currentBalance = newBalance;
        rebuildAccountSummaries();
    } else {
        loadTransactionsFromCSV(0);   /* file was missing before; sync memory with it */
    }

    return 1;
}

/* Returns 1 if another transaction row fits in memory. */
static int hasTxnCapacity(void) {
    if (g_txnCount >= MAX_TXNS) {
        printf("Transaction storage is full (%d). Cannot record more transactions.\n", MAX_TXNS);
        return 0;
    }
    return 1;
}

/* Menu [1]: Register Account */
void handleRegisterAccount(void) {
    if (!g_dataLoaded) {
        loadTransactionsFromCSV(0);
    }

    do {
        char name[MAX_NAME];
        double initDeposit = 0.0;
        int r, cancelled = 0;

        printf("\nRegister Account\n");
        r = promptName("Account Name (First Last): ", name, sizeof(name));
        if (r == IN_EOF) {
            break;
        }

        if (r == IN_BLANK) {
            printf("Registration cancelled.\n");
        } else if (!isValidFullName(name)) {
            printf("Invalid name. Use a first and last name (letters, spaces, hyphens, apostrophes only).\n");
        } else if (findAccountByName(name) != -1) {
            printf("An account with that name already exists.\n");
        } else if (g_accountCount >= MAX_ACCOUNTS) {
            printf("Account storage is full (%d accounts).\n", MAX_ACCOUNTS);
        } else if (hasTxnCapacity()) {
            for (;;) {
                r = promptDouble("Initial Deposit (blank to cancel): ", &initDeposit);
                if (r == IN_EOF || r == IN_BLANK) {
                    cancelled = 1;
                    break;
                }
                if (r == IN_OK && initDeposit >= MIN_INITIAL_DEPOSIT) {
                    break;
                }
                printf("Invalid deposit. Minimum initial deposit is Php. 5,000.00.\n");
            }

            if (cancelled) {
                printf("Registration cancelled.\n");
            } else {
                long long newNumber = g_maxAccountNumber + 1;
                initDeposit = roundCents(initDeposit);
                if (appendTransactionToCSV(name, newNumber, "deposit", initDeposit, initDeposit)) {
                    g_maxAccountNumber = newNumber;
                    printf("\nAccount created successfully! (Account Number: %lld)\n", newNumber);
                }
            }
        }
    } while (!g_inputEnded && promptYesNo("\nRegister another account? (Y/N): "));
}

/* Menu [2]: Deposit Amount */
void handleDepositAmount(void) {
    if (!g_dataLoaded) {
        loadTransactionsFromCSV(0);
    }

    do {
        char name[MAX_NAME];
        double deposit = 0.0;
        int idx, r;

        printf("\nDeposit Amount\n");
        r = promptName("Account Name: ", name, sizeof(name));
        if (r == IN_EOF) {
            break;
        }

        idx = (r == IN_OK) ? findAccountByName(name) : -1;
        if (idx == -1) {
            printf("Account not found.\n");
        } else {
            printf("Current Balance: %.2f\n", g_accounts[idx].currentBalance);
            printf("Currency: PHP\n\n");
            r = promptDouble("Deposit Amount: ", &deposit);
            if (r == IN_OK && deposit > 0.0 && hasTxnCapacity()) {
                double newBal = roundCents(g_accounts[idx].currentBalance + roundCents(deposit));
                char accName[MAX_NAME];
                long long accNum = g_accounts[idx].accountNumber;
                snprintf(accName, sizeof(accName), "%s", g_accounts[idx].accountName);
                if (appendTransactionToCSV(accName, accNum, "deposit", roundCents(deposit), newBal)) {
                    printf("\nUpdated Balance: %.2f\n", newBal);
                }
            } else if (r != IN_EOF && !(r == IN_OK && deposit > 0.0)) {
                printf("Invalid deposit amount.\n");
            }
        }
    } while (!g_inputEnded && promptYesNo("\nDeposit another amount? (Y/N): "));
}

/* Menu [3]: Withdraw Amount */
void handleWithdrawAmount(void) {
    if (!g_dataLoaded) {
        loadTransactionsFromCSV(0);
    }

    do {
        char name[MAX_NAME];
        double withdraw = 0.0;
        int idx, r;

        printf("\nWithdraw Amount\n");
        r = promptName("Account Name: ", name, sizeof(name));
        if (r == IN_EOF) {
            break;
        }

        idx = (r == IN_OK) ? findAccountByName(name) : -1;
        if (idx == -1) {
            printf("Account not found.\n");
        } else {
            printf("Current Balance: %.2f\n", g_accounts[idx].currentBalance);
            printf("Currency: PHP\n\n");
            r = promptDouble("Withdraw Amount: ", &withdraw);
            if (r == IN_OK && withdraw > 0.0) {
                withdraw = roundCents(withdraw);
                if (withdraw > g_accounts[idx].currentBalance) {
                    printf("Insufficient balance.\n");
                } else if (hasTxnCapacity()) {
                    double newBal = roundCents(g_accounts[idx].currentBalance - withdraw);
                    char accName[MAX_NAME];
                    long long accNum = g_accounts[idx].accountNumber;
                    snprintf(accName, sizeof(accName), "%s", g_accounts[idx].accountName);
                    if (appendTransactionToCSV(accName, accNum, "withdrawal", withdraw, newBal)) {
                        printf("\nUpdated Balance: %.2f\n", newBal);
                    }
                }
            } else if (r != IN_EOF) {
                printf("Invalid withdrawal amount.\n");
            }
        }
    } while (!g_inputEnded && promptYesNo("\nWithdraw another amount? (Y/N): "));
}

/* Menu [4]: Currency Exchange */
void handleCurrencyExchange(void) {
    do {
        int srcCurrency = 0, destCurrency = 0;
        double srcAmount = 0.0, destAmount = 0.0;

        printf("\nForeign Currency Exchange\n");
        printf("Source Currency Option:\n");
        printCurrencyOptions();

        if (promptInt("\nSource Currency: ", &srcCurrency) != IN_OK || srcCurrency < 1 || srcCurrency > 6) {
            printf("Invalid currency option.\n");
            break;
        }

        if (promptDouble("Source Amount: ", &srcAmount) != IN_OK || srcAmount < 0.0) {
            printf("Invalid amount.\n");
            break;
        }

        printf("\nExchanged Currency Options:\n");
        printCurrencyOptions();

        if (promptInt("\nExchange Currency: ", &destCurrency) != IN_OK || destCurrency < 1 || destCurrency > 6) {
            printf("Invalid currency option.\n");
            break;
        }

        destAmount = (srcAmount * g_rates[srcCurrency]) / g_rates[destCurrency];
        printf("Exchange Amount: %.2f\n", destAmount);
    } while (!g_inputEnded && promptYesNo("\nConvert another currency (Y/N)? "));
}

/* Menu [5]: Record Exchange Rates */
void handleRecordExchangeRates(void) {
    int currencyIndex = 0;
    double newRate = 0.0;

    printf("\nRecord Exchange Rate\n");
    printCurrencyOptions();

    if (promptInt("\nSelect Foreign Currency: ", &currencyIndex) != IN_OK ||
        currencyIndex < 2 || currencyIndex > 6) {
        printf("Invalid currency option (PHP is the base currency and cannot be changed).\n");
        return;
    }

    if (promptDouble("Exchange Rate: ", &newRate) == IN_OK && newRate > 0.0) {
        g_rates[currencyIndex] = newRate;
        printf("Exchange rate recorded.\n");
    } else {
        printf("Invalid exchange rate.\n");
    }
}

/* Menu [6]: Show Interest Amount */
void handleShowInterestAmount(void) {
    char name[MAX_NAME];
    int days = 0;
    int i, idx;
    double tempBalance;

    if (!g_dataLoaded) {
        loadTransactionsFromCSV(0);
    }

    printf("\nShow Interest Amount\n");
    if (promptName("Account Name: ", name, sizeof(name)) != IN_OK ||
        (idx = findAccountByName(name)) == -1) {
        printf("Account not found.\n");
        return;
    }

    tempBalance = g_accounts[idx].currentBalance;
    printf("Current Balance: %.2f\n", tempBalance);
    printf("Currency: PHP\n");
    printf("Interest Rate: 5%%\n");

    if (promptInt("Total Number of Days: ", &days) == IN_OK && days > 0 && days <= 3650) {
        printf("\nDay\t| Interest\t| Balance\t|\n");
        for (i = 1; i <= days; i++) {
            double di = dailyInterest(tempBalance);
            tempBalance += di;
            printf("%d\t| %.2f\t\t| %.2f\t|\n", i, di, tempBalance);
        }
    } else {
        printf("Invalid number of days (enter 1 to 3650).\n");
    }
}

/* ------------------------------------------------------------------ */
/* Reports                                                            */
/* ------------------------------------------------------------------ */

/* Comparator for Report 1: descending by AverageDailyBalance. */
int cmpReport1(const void *a, const void *b) {
    const AccountSummary *x = (const AccountSummary *)a;
    const AccountSummary *y = (const AccountSummary *)b;
    if (x->avgDailyBalance < y->avgDailyBalance) return 1;
    if (x->avgDailyBalance > y->avgDailyBalance) return -1;
    return compareNames(x->accountName, y->accountName);
}

/* Comparator for Report 2: descending by ExpectedInterest. */
int cmpReport2(const void *a, const void *b) {
    const AccountSummary *x = (const AccountSummary *)a;
    const AccountSummary *y = (const AccountSummary *)b;
    if (x->expectedInterest < y->expectedInterest) return 1;
    if (x->expectedInterest > y->expectedInterest) return -1;
    return compareNames(x->accountName, y->accountName);
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
    riskCmp = strcmp(y->riskFlag, x->riskFlag);   /* "Suspicious" sorts before "Normal" */
    if (riskCmp != 0) {
        return riskCmp;
    }
    return compareNames(x->accountName, y->accountName);
}

/* Comparator for grouping transactions by account, then chronologically (ties by file order). */
int cmpTxnByAccDate(const void *a, const void *b) {
    const Transaction *x = (const Transaction *)a;
    const Transaction *y = (const Transaction *)b;
    int nameCmp = compareNames(x->accountName, y->accountName);
    int dateCmp, secX, secY;
    if (nameCmp != 0) {
        return nameCmp;
    }
    dateCmp = compareDates(x->day, x->month, x->year, y->day, y->month, y->year);
    if (dateCmp != 0) {
        return dateCmp;
    }
    secX = x->hour * 3600 + x->minute * 60 + x->second;
    secY = y->hour * 3600 + y->minute * 60 + y->second;
    if (secX != secY) {
        return secX - secY;
    }
    return x->seq - y->seq;
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

        for (d = 0; d < 32; d++) {
            eodBalance[d] = 0.0;
            hasTxnOnDay[d] = 0;
        }

        for (j = 0; j < g_txnCount; j++) {
            if (compareNames(sortedTxns[j].accountName, acc.accountName) != 0) {
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
            writeCsvText(fp, g_report1[i].accountName);
            fprintf(fp, ",%d,\"%s\",\"%s\",%s\n",
                    g_report1[i].accountAge, balBuf, adbBuf, g_report1[i].privilegeTier);
        }
        fclose(fp);
    } else {
        printf("Warning: could not write report1_banking_privilege_tiers.csv\n");
    }

    /* Report 2: Interest Expense Projection */
    g_report2Count = 0;
    for (i = 0; i < g_accountCount; i++) {
        AccountSummary acc = g_accounts[i];
        totalBalance += acc.currentBalance;

        if (acc.currentBalance >= 10000.00) {
            acc.expectedInterest = projectInterest(acc.currentBalance, 30);
            totalInterestExpense += acc.expectedInterest;
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
            writeCsvText(fp, g_report2[i].accountName);
            fprintf(fp, ",\"%s\",\"%s\"\n", balBuf, intBuf);
        }
        fclose(fp);
    } else {
        printf("Warning: could not write report2_interest_expense_projection.csv\n");
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
               compareNames(sortedTxns[i].accountName, rec.accountName) == 0 &&
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
            rec.turnoverVelocity = 0.0;   /* undefined without deposits; see ASSUMPTIONS */
        }

        if (rec.totalWithdrawals >= 500000.00 || rec.turnoverVelocity >= 90.0) {
            snprintf(rec.riskFlag, sizeof(rec.riskFlag), "Suspicious");
        } else {
            snprintf(rec.riskFlag, sizeof(rec.riskFlag), "Normal");
        }

        g_report3[g_report3Count++] = rec;
    }
    if (i < g_txnCount) {
        printf("Warning: Report 3 capacity (%d rows) reached; remaining days omitted.\n", MAX_DAILY_RECS);
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
            writeCsvText(fp, g_report3[i].accountName);
            fprintf(fp, ",%02d/%02d/%04d,\"%s\",%d,\"%s\",\"%s\",%s\n",
                    g_report3[i].day, g_report3[i].month, g_report3[i].year,
                    wBuf, g_report3[i].numDeposits, dBuf, vBuf, g_report3[i].riskFlag);
        }
        fclose(fp);
    } else {
        printf("Warning: could not write report3_suspicious_client_behavior.csv\n");
    }

    /* Summary JSON */
    fp = fopen("summary.json", "w");
    if (fp != NULL) {
        fprintf(fp, "{\"total_accounts\": %d, \"total_deposits\": %.2f, \"total_withdrawals\": %.2f, \"total_balance\": %.2f, \"total_interest_expense\": %.2f}\n",
                g_accountCount, totalDeposits, totalWithdrawals, totalBalance, totalInterestExpense);
        fclose(fp);
    } else {
        printf("Warning: could not write summary.json\n");
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

    if (promptInt("Enter choice: ", &subChoice) != IN_OK) {
        printf("Invalid choice.\n");
        return;
    }

    if (subChoice == 1) {
        if (!loadTransactionsFromCSV(1)) {
            printf("Could not load %s.\n", CSV_FILENAME);
        }
    } else if (subChoice == 2) {
        generateAllReports();
    } else {
        printf("Invalid choice.\n");
    }
}

int main(void) {
    int choice;

    do {
        printf("\nSelect Transaction:\n");
        printf("[1] Register Account\n");
        printf("[2] Deposit Amount\n");
        printf("[3] Withdraw Amount\n");
        printf("[4] Currency Exchange\n");
        printf("[5] Record Exchange Rates\n");
        printf("[6] Show Interest Amount\n");
        printf("[7] Generate Reports\n");

        if (promptInt("\nChoice: ", &choice) != IN_OK) {
            if (g_inputEnded) {
                break;
            }
            choice = 0;
        }

        switch (choice) {
            case 1: handleRegisterAccount(); break;
            case 2: handleDepositAmount(); break;
            case 3: handleWithdrawAmount(); break;
            case 4: handleCurrencyExchange(); break;
            case 5: handleRecordExchangeRates(); break;
            case 6: handleShowInterestAmount(); break;
            case 7: handleGenerateReports(); break;
            default: printf("Invalid choice. Please select 1-7.\n"); break;
        }
    } while (!g_inputEnded && promptYesNo("\nBack to the Main Menu (Y/N): "));

    return 0;
}
