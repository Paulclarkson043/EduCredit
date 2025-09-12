;; ScholarshipFund - Community Scholarship Management System
;; Allows sponsors to create scholarship funds and students to apply for scholarships

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-fund-not-found (err u201))
(define-constant err-insufficient-funds (err u202))
(define-constant err-unauthorized (err u203))
(define-constant err-application-not-found (err u204))
(define-constant err-invalid-amount (err u205))
(define-constant err-fund-inactive (err u206))
(define-constant err-application-exists (err u207))
(define-constant err-application-processed (err u208))
(define-constant err-already-withdrawn (err u209))

;; Data Variables
(define-data-var fund-nonce uint u0)
(define-data-var application-nonce uint u0)

;; Data Maps

;; Scholarship funds created by sponsors
(define-map scholarship-funds
    uint ;; fund-id
    {
        sponsor: principal,
        fund-name: (string-ascii 50),
        fund-balance: uint,
        scholarship-amount: uint,
        max-scholarships: uint,
        awarded-count: uint,
        active: bool,
        creation-date: uint,
        application-deadline: uint
    }
)

;; Student applications for scholarships
(define-map scholarship-applications
    uint ;; application-id
    {
        applicant: principal,
        fund-id: uint,
        student-name: (string-ascii 50),
        essay: (string-ascii 500),
        gpa: uint, ;; scaled by 100 (e.g., 350 = 3.50 GPA)
        status: (string-ascii 10), ;; "pending", "approved", "denied"
        application-date: uint,
        processed-date: uint,
        withdrawn: bool
    }
)

;; Track applications per fund for easy lookup
(define-map fund-applications
    uint ;; fund-id
    {
        application-list: (list 100 uint)
    }
)

;; Track student applications to prevent duplicates
(define-map student-fund-applications
    {student: principal, fund-id: uint}
    uint ;; application-id
)

;; Private Functions
(define-private (is-owner)
    (is-eq tx-sender contract-owner)
)

;; Public Functions

;; Create a new scholarship fund
(define-public (create-fund 
    (fund-name (string-ascii 50))
    (scholarship-amount uint)
    (max-scholarships uint)
    (application-deadline uint))
    (let
        ((fund-id (var-get fund-nonce))
         (initial-deposit scholarship-amount))
        
        (asserts! (> scholarship-amount u0) err-invalid-amount)
        (asserts! (> max-scholarships u0) err-invalid-amount)
        (asserts! (> application-deadline stacks-block-height) err-invalid-amount)
        
        ;; Transfer initial funding from sponsor
        (try! (stx-transfer? initial-deposit tx-sender (as-contract tx-sender)))
        
        (map-set scholarship-funds fund-id
            {
                sponsor: tx-sender,
                fund-name: fund-name,
                fund-balance: initial-deposit,
                scholarship-amount: scholarship-amount,
                max-scholarships: max-scholarships,
                awarded-count: u0,
                active: true,
                creation-date: stacks-block-height,
                application-deadline: application-deadline
            }
        )
        
        (map-set fund-applications fund-id
            {
                application-list: (list)
            }
        )
        
        (var-set fund-nonce (+ fund-id u1))
        (ok fund-id)
    )
)

;; Add additional funding to existing scholarship fund
(define-public (contribute-fund (fund-id uint) (amount uint))
    (let
        ((fund (unwrap! (map-get? scholarship-funds fund-id) err-fund-not-found)))
        
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (get active fund) err-fund-inactive)
        
        ;; Transfer contribution
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        (map-set scholarship-funds fund-id
            (merge fund
                {
                    fund-balance: (+ (get fund-balance fund) amount)
                }
            )
        )
        (ok true)
    )
)

;; Student applies for scholarship
(define-public (apply-scholarship
    (fund-id uint)
    (student-name (string-ascii 50))
    (essay (string-ascii 500))
    (gpa uint))
    (let
        ((fund (unwrap! (map-get? scholarship-funds fund-id) err-fund-not-found))
         (application-id (var-get application-nonce))
         (current-applications (default-to {application-list: (list)} 
             (map-get? fund-applications fund-id))))
        
        (asserts! (get active fund) err-fund-inactive)
        (asserts! (<= stacks-block-height (get application-deadline fund)) err-invalid-amount)
        (asserts! (is-none (map-get? student-fund-applications {student: tx-sender, fund-id: fund-id})) err-application-exists)
        (asserts! (<= gpa u400) err-invalid-amount) ;; Max GPA 4.00
        
        ;; Record the application
        (map-set scholarship-applications application-id
            {
                applicant: tx-sender,
                fund-id: fund-id,
                student-name: student-name,
                essay: essay,
                gpa: gpa,
                status: "pending",
                application-date: stacks-block-height,
                processed-date: u0,
                withdrawn: false
            }
        )
        
        ;; Track student application to prevent duplicates
        (map-set student-fund-applications {student: tx-sender, fund-id: fund-id} application-id)
        
        ;; Add to fund's application list
        (map-set fund-applications fund-id
            {
                application-list: (unwrap-panic (as-max-len? 
                    (append (get application-list current-applications) application-id) u100))
            }
        )
        
        (var-set application-nonce (+ application-id u1))
        (ok application-id)
    )
)

;; Sponsor processes (approves/denies) scholarship application
(define-public (process-application (application-id uint) (approved bool))
    (let
        ((application (unwrap! (map-get? scholarship-applications application-id) err-application-not-found))
         (fund (unwrap! (map-get? scholarship-funds (get fund-id application)) err-fund-not-found)))
        
        (asserts! (is-eq tx-sender (get sponsor fund)) err-unauthorized)
        (asserts! (is-eq (get status application) "pending") err-application-processed)
        (asserts! (get active fund) err-fund-inactive)
        
        (if approved
            (begin
                ;; Check if fund has capacity and balance
                (asserts! (< (get awarded-count fund) (get max-scholarships fund)) err-insufficient-funds)
                (asserts! (>= (get fund-balance fund) (get scholarship-amount fund)) err-insufficient-funds)
                
                ;; Update fund awarded count
                (map-set scholarship-funds (get fund-id application)
                    (merge fund
                        {
                            awarded-count: (+ (get awarded-count fund) u1)
                        }
                    )
                )
                
                ;; Approve application
                (map-set scholarship-applications application-id
                    (merge application
                        {
                            status: "approved",
                            processed-date: stacks-block-height
                        }
                    )
                )
            )
            ;; Deny application
            (map-set scholarship-applications application-id
                (merge application
                    {
                        status: "denied",
                        processed-date: stacks-block-height
                    }
                )
            )
        )
        (ok true)
    )
)

;; Approved student withdraws scholarship funds
(define-public (withdraw-scholarship (application-id uint))
    (let
        ((application (unwrap! (map-get? scholarship-applications application-id) err-application-not-found))
         (fund (unwrap! (map-get? scholarship-funds (get fund-id application)) err-fund-not-found)))
        
        (asserts! (is-eq tx-sender (get applicant application)) err-unauthorized)
        (asserts! (is-eq (get status application) "approved") err-unauthorized)
        (asserts! (not (get withdrawn application)) err-already-withdrawn)
        (asserts! (>= (get fund-balance fund) (get scholarship-amount fund)) err-insufficient-funds)
        
        ;; Transfer scholarship to student
        (try! (as-contract (stx-transfer? (get scholarship-amount fund) tx-sender (get applicant application))))
        
        ;; Update fund balance
        (map-set scholarship-funds (get fund-id application)
            (merge fund
                {
                    fund-balance: (- (get fund-balance fund) (get scholarship-amount fund))
                }
            )
        )
        
        ;; Mark application as withdrawn
        (map-set scholarship-applications application-id
            (merge application
                {
                    withdrawn: true
                }
            )
        )
        
        (ok true)
    )
)

;; Sponsor deactivates fund (prevents new applications)
(define-public (deactivate-fund (fund-id uint))
    (let
        ((fund (unwrap! (map-get? scholarship-funds fund-id) err-fund-not-found)))
        
        (asserts! (is-eq tx-sender (get sponsor fund)) err-unauthorized)
        
        (map-set scholarship-funds fund-id
            (merge fund
                {
                    active: false
                }
            )
        )
        (ok true)
    )
)

;; Read-only Functions
(define-read-only (get-fund-info (fund-id uint))
    (map-get? scholarship-funds fund-id)
)

(define-read-only (get-application-info (application-id uint))
    (map-get? scholarship-applications application-id)
)

(define-read-only (get-fund-applications (fund-id uint))
    (map-get? fund-applications fund-id)
)

(define-read-only (get-student-application (student principal) (fund-id uint))
    (map-get? student-fund-applications {student: student, fund-id: fund-id})
)

(define-read-only (get-contract-balance)
    (stx-get-balance (as-contract tx-sender))
)
