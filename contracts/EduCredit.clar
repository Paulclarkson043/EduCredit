;; EduCredit - Educational Credit Transfer Platform
;; Core features: Institution management, course verification, credit transfer

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-invalid-credits (err u103))
(define-constant err-course-exists (err u104))
(define-constant err-course-not-found (err u105))
(define-constant err-unauthorized (err u106))
(define-constant err-transfer-not-found (err u107))
(define-constant err-invalid-rating (err u108))
;; Data Variables
(define-data-var subscription-fee uint u100)

;; Data Maps
(define-map registered-institutions 
    principal 
    {
        name: (string-ascii 50),
        verified: bool,
        join-date: uint
    }
)

(define-map courses 
    {institution: principal, course-id: (string-ascii 20)}
    {
        course-name: (string-ascii 50),
        credits: uint,
        verified: bool
    }
)

(define-map credit-transfers
    uint 
    {
        from-institution: principal,
        to-institution: principal,
        student-id: (string-ascii 20),
        course-id: (string-ascii 20),
        credits: uint,
        status: (string-ascii 10),
        timestamp: uint
    }
)

(define-data-var transfer-nonce uint u0)

;; Private Functions
(define-private (is-owner)
    (is-eq tx-sender contract-owner)
)

(define-private (is-registered (institution principal))
    (default-to false (get verified (map-get? registered-institutions institution)))
)

;; Public Functions
(define-public (register-institution (name (string-ascii 50)))
    (begin
        (asserts! (not (is-registered tx-sender)) err-already-registered)
        (map-set registered-institutions tx-sender
            {
                name: name,
                verified: false,
                join-date: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (verify-institution (institution principal))
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (is-some (map-get? registered-institutions institution)) err-not-registered)
        (map-set registered-institutions institution
            (merge (unwrap-panic (map-get? registered-institutions institution))
                {verified: true}
            )
        )
        (ok true)
    )
)

(define-public (add-course (course-id (string-ascii 20)) (course-name (string-ascii 50)) (credits uint))
    (begin
        (asserts! (is-registered tx-sender) err-not-registered)
        (asserts! (> credits u0) err-invalid-credits)
        (asserts! (is-none (map-get? courses {institution: tx-sender, course-id: course-id})) err-course-exists)
        (map-set courses {institution: tx-sender, course-id: course-id}
            {
                course-name: course-name,
                credits: credits,
                verified: false
            }
        )
        (ok true)
    )
)

(define-public (verify-course (institution principal) (course-id (string-ascii 20)))
    (begin
        (asserts! (is-owner) err-owner-only)
         (asserts! (is-none (map-get? courses {institution: tx-sender, course-id: course-id})) err-course-exists)           
            (map-set courses {institution: institution, course-id: course-id}
            (merge (unwrap-panic (map-get? courses {institution: institution, course-id: course-id}))
                {verified: true}
            )
        )
        (ok true)
    )
)

(define-public (initiate-credit-transfer 
    (to-institution principal)
    (student-id (string-ascii 20))
    (course-id (string-ascii 20)))
    (let
        (
            (course (unwrap! (map-get? courses {institution: tx-sender, course-id: course-id}) err-course-not-found))
            (nonce (var-get transfer-nonce))
        )
        (asserts! (is-registered tx-sender) err-not-registered)
        (asserts! (is-registered to-institution) err-not-registered)
        (asserts! (get verified course) err-unauthorized)
        
        (map-set credit-transfers nonce
            {
                from-institution: tx-sender,
                to-institution: to-institution,
                student-id: student-id,
                course-id: course-id,
                credits: (get credits course),
                status: "pending",
                timestamp: stacks-block-height
            }
        )
        (var-set transfer-nonce (+ nonce u1))
        (ok nonce)
    )
)

(define-public (accept-credit-transfer (transfer-id uint))
    (let
        ((transfer (unwrap! (map-get? credit-transfers transfer-id) err-course-not-found)))
        (asserts! (is-eq tx-sender (get to-institution transfer)) err-unauthorized)
        (map-set credit-transfers transfer-id
            (merge transfer {status: "accepted"})
        )
        (ok true)
    )
)

;; Smart Credit Portfolio & Degree Progress Tracking System

;; Additional error constants for portfolio system
(define-constant err-degree-not-found (err u115))
(define-constant err-invalid-credit-category (err u116))
(define-constant err-degree-already-exists (err u117))
(define-constant err-portfolio-not-found (err u118))
(define-constant err-insufficient-credits (err u119))

;; Degree requirement templates
(define-map degree-templates
    {institution: principal, degree-id: (string-ascii 30)}
    {
        degree-name: (string-ascii 50),
        total-credits-required: uint,
        core-credits-required: uint,
        elective-credits-required: uint,
        major-credits-required: uint,
        minimum-gpa: uint, ;; scaled by 100 (e.g., 250 = 2.50 GPA)
        active: bool
    }
)

;; Student credit portfolios tracking all credits across institutions
(define-map student-portfolios
    (string-ascii 20) ;; student-id
    {
        total-credits: uint,
        core-credits: uint,
        elective-credits: uint,
        major-credits: uint,
        institutions-attended: (list 20 principal),
        current-gpa: uint, ;; scaled by 100
        last-updated: uint
    }
)

;; Individual credit records within portfolios
(define-map portfolio-credits
    {student-id: (string-ascii 20), credit-id: uint}
    {
        institution: principal,
        course-id: (string-ascii 20),
        credits: uint,
        grade: (string-ascii 2),
        grade-points: uint, ;; scaled by 100
        credit-category: (string-ascii 10), ;; "core", "major", "elective"
        completion-date: uint,
        verified: bool
    }
)

;; Degree progress tracking for students
(define-map degree-progress
    {student-id: (string-ascii 20), institution: principal, degree-id: (string-ascii 30)}
    {
        progress-percentage: uint,
        core-completion: uint,
        major-completion: uint,
        elective-completion: uint,
        estimated-completion-date: uint,
        requirements-met: bool,
        last-calculated: uint
    }
)

(define-data-var portfolio-credit-nonce uint u0)

;; Private helper functions
(define-private (calculate-grade-points (grade (string-ascii 2)) (credits uint))
    (let ((grade-value (if (is-eq grade "A+") u425
                      (if (is-eq grade "A") u400
                      (if (is-eq grade "A-") u375
                      (if (is-eq grade "B+") u350
                      (if (is-eq grade "B") u300
                      (if (is-eq grade "B-") u275
                      (if (is-eq grade "C+") u250
                      (if (is-eq grade "C") u200
                      (if (is-eq grade "D") u100
                      u0)))))))))))
        (* grade-value credits)
    )
)

(define-private (is-valid-credit-category (category (string-ascii 10)))
    (or (is-eq category "core")
        (or (is-eq category "major") (is-eq category "elective")))
)

(define-private (min-uint (a uint) (b uint))
    (if (< a b) a b)
)

;; Public functions for degree template management
(define-public (create-degree-template
    (degree-id (string-ascii 30))
    (degree-name (string-ascii 50))
    (total-credits uint)
    (core-credits uint)
    (major-credits uint)
    (elective-credits uint)
    (min-gpa uint))
    (begin
        ;; Only registered institutions can create degree templates
        (asserts! (is-registered tx-sender) err-not-registered)
        (asserts! (is-none (map-get? degree-templates {institution: tx-sender, degree-id: degree-id})) err-degree-already-exists)
        (asserts! (is-eq total-credits (+ core-credits (+ major-credits elective-credits))) err-invalid-credits)
        (asserts! (<= min-gpa u400) err-invalid-rating) ;; Max GPA 4.00
        
        (map-set degree-templates {institution: tx-sender, degree-id: degree-id}
            {
                degree-name: degree-name,
                total-credits-required: total-credits,
                core-credits-required: core-credits,
                elective-credits-required: elective-credits,
                major-credits-required: major-credits,
                minimum-gpa: min-gpa,
                active: true
            }
        )
        (ok true)
    )
)

;; Function to record a credit in student's portfolio
(define-public (record-portfolio-credit
    (student-id (string-ascii 20))
    (course-id (string-ascii 20))
    (credits uint)
    (grade (string-ascii 2))
    (credit-category (string-ascii 10)))
    (let
        ((credit-id (var-get portfolio-credit-nonce))
         (grade-points (calculate-grade-points grade credits))
         (current-portfolio (default-to 
            {
                total-credits: u0,
                core-credits: u0,
                elective-credits: u0,
                major-credits: u0,
                institutions-attended: (list),
                current-gpa: u0,
                last-updated: u0
            }
            (map-get? student-portfolios student-id))))
        
        ;; Validate inputs
        (asserts! (is-registered tx-sender) err-not-registered)
        (asserts! (> credits u0) err-invalid-credits)
        (asserts! (is-valid-credit-category credit-category) err-invalid-credit-category)
        
        ;; Record the individual credit
        (map-set portfolio-credits {student-id: student-id, credit-id: credit-id}
            {
                institution: tx-sender,
                course-id: course-id,
                credits: credits,
                grade: grade,
                grade-points: grade-points,
                credit-category: credit-category,
                completion-date: stacks-block-height,
                verified: true
            }
        )
        
        ;; Update student's portfolio summary
        (map-set student-portfolios student-id
            (merge current-portfolio
                {
                    total-credits: (+ (get total-credits current-portfolio) credits),
                    core-credits: (if (is-eq credit-category "core")
                        (+ (get core-credits current-portfolio) credits)
                        (get core-credits current-portfolio)),
                    major-credits: (if (is-eq credit-category "major")
                        (+ (get major-credits current-portfolio) credits)
                        (get major-credits current-portfolio)),
                    elective-credits: (if (is-eq credit-category "elective")
                        (+ (get elective-credits current-portfolio) credits)
                        (get elective-credits current-portfolio)),
                    last-updated: stacks-block-height
                }
            )
        )
        
        (var-set portfolio-credit-nonce (+ credit-id u1))
        (ok credit-id)
    )
)

;; Function to calculate degree progress for a student
(define-public (calculate-degree-progress
    (student-id (string-ascii 20))
    (institution principal)
    (degree-id (string-ascii 30)))
    (let
        ((degree-template (unwrap! (map-get? degree-templates {institution: institution, degree-id: degree-id}) err-degree-not-found))
         (portfolio (unwrap! (map-get? student-portfolios student-id) err-portfolio-not-found))
         (core-progress (/ (* (get core-credits portfolio) u100) (get core-credits-required degree-template)))
         (major-progress (/ (* (get major-credits portfolio) u100) (get major-credits-required degree-template)))
         (elective-progress (/ (* (get elective-credits portfolio) u100) (get elective-credits-required degree-template)))
         (total-progress (/ (* (get total-credits portfolio) u100) (get total-credits-required degree-template)))
         (requirements-met (and
            (>= (get core-credits portfolio) (get core-credits-required degree-template))
            (and
                (>= (get major-credits portfolio) (get major-credits-required degree-template))
                (>= (get elective-credits portfolio) (get elective-credits-required degree-template))))))
        
        ;; Ensure the degree template is active
        (asserts! (get active degree-template) err-unauthorized)
        
        (map-set degree-progress {student-id: student-id, institution: institution, degree-id: degree-id}
            {
                progress-percentage: (min-uint total-progress u100),
                core-completion: (min-uint core-progress u100),
                major-completion: (min-uint major-progress u100),
                elective-completion: (min-uint elective-progress u100),
                estimated-completion-date: (if requirements-met
                    stacks-block-height
                    (+ stacks-block-height u26280)), ;; ~6 months estimate
                requirements-met: requirements-met,
                last-calculated: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Function to validate degree completion eligibility
(define-public (validate-degree-completion
    (student-id (string-ascii 20))
    (institution principal)
    (degree-id (string-ascii 30)))
    (let
        ((degree-template (unwrap! (map-get? degree-templates {institution: institution, degree-id: degree-id}) err-degree-not-found))
         (portfolio (unwrap! (map-get? student-portfolios student-id) err-portfolio-not-found))
         (progress (unwrap! (map-get? degree-progress {student-id: student-id, institution: institution, degree-id: degree-id}) err-degree-not-found)))
        
        ;; Check all degree requirements
        (asserts! (>= (get total-credits portfolio) (get total-credits-required degree-template)) err-insufficient-credits)
        (asserts! (>= (get core-credits portfolio) (get core-credits-required degree-template)) err-insufficient-credits)
        (asserts! (>= (get major-credits portfolio) (get major-credits-required degree-template)) err-insufficient-credits)
        (asserts! (>= (get elective-credits portfolio) (get elective-credits-required degree-template)) err-insufficient-credits)
        (asserts! (>= (get current-gpa portfolio) (get minimum-gpa degree-template)) err-invalid-rating)
        
        (ok true)
    )
)

;; Read-only Functions
(define-read-only (get-institution-info (institution principal))
    (map-get? registered-institutions institution)
)

(define-read-only (get-course-info (institution principal) (course-id (string-ascii 20)))
    (map-get? courses {institution: institution, course-id: course-id})
)

(define-read-only (get-transfer-info (transfer-id uint))
    (map-get? credit-transfers transfer-id)
)


(define-map registered-students
    {student-id: (string-ascii 20)}
    {
        name: (string-ascii 50),
        institution: principal,
        enrollment-date: uint,
        active: bool
    }
)

(define-public (register-student 
    (student-id (string-ascii 20))
    (name (string-ascii 50)))
    (begin
        (asserts! (is-registered tx-sender) err-not-registered)
        (map-set registered-students {student-id: student-id}
            {
                name: name,
                institution: tx-sender,
                enrollment-date: stacks-block-height,
                active: true
            }
        )
        (ok true)
    )
)


(define-map course-ratings
    {institution: principal, course-id: (string-ascii 20)}
    {
        total-rating: uint,
        number-of-ratings: uint,
        average-rating: uint
    }
)

(define-public (rate-course 
    (institution principal)
    (course-id (string-ascii 20))
    (rating uint))
    (let
        ((current-ratings (default-to 
            {total-rating: u0, number-of-ratings: u0, average-rating: u0}
            (map-get? course-ratings {institution: institution, course-id: course-id}))))
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-credits)
        (map-set course-ratings {institution: institution, course-id: course-id}
            {
                total-rating: (+ (get total-rating current-ratings) rating),
                number-of-ratings: (+ (get number-of-ratings current-ratings) u1),
                average-rating: (/ (+ (get total-rating current-ratings) rating) 
                                 (+ (get number-of-ratings current-ratings) u1))
            }
        )
        (ok true)
    )
)

(define-map transfer-history
    principal
    {
        transfers-sent: (list 50 uint),
        transfers-received: (list 50 uint)
    }
)

(define-public (add-transfer-to-history (transfer-id uint))
    (let
        ((transfer (unwrap! (map-get? credit-transfers transfer-id) err-course-not-found))
         (from-history (default-to {transfers-sent: (list), transfers-received: (list)} 
            (map-get? transfer-history (get from-institution transfer))))
         (to-history (default-to {transfers-sent: (list), transfers-received: (list)} 
            (map-get? transfer-history (get to-institution transfer)))))
        (map-set transfer-history (get from-institution transfer)
            (merge from-history {transfers-sent: (unwrap-panic (as-max-len? 
                (append (get transfers-sent from-history) transfer-id) u50))}))
        (map-set transfer-history (get to-institution transfer)
            (merge to-history {transfers-received: (unwrap-panic (as-max-len? 
                (append (get transfers-received to-history) transfer-id) u50))}))
        (ok true)
    )
)


(define-map course-prerequisites
    {institution: principal, course-id: (string-ascii 20)}
    {prerequisites: (list 10 (string-ascii 20))}
)

(define-public (set-course-prerequisites 
    (course-id (string-ascii 20))
    (prereq-list (list 10 (string-ascii 20))))
    (begin
        (asserts! (is-registered tx-sender) err-not-registered)
        (asserts! (is-some (map-get? courses {institution: tx-sender, course-id: course-id})) err-course-not-found)
        (map-set course-prerequisites {institution: tx-sender, course-id: course-id}
            {prerequisites: prereq-list}
        )
        (ok true)
    )
)

(define-map academic-calendar
    principal
    {
        semester-start: uint,
        semester-end: uint,
        registration-deadline: uint,
        transfer-deadline: uint
    }
)

(define-public (set-academic-calendar 
    (start uint)
    (end uint)
    (reg-deadline uint)
    (transfer-deadline uint))
    (begin
        (asserts! (is-registered tx-sender) err-not-registered)
        (map-set academic-calendar tx-sender
            {
                semester-start: start,
                semester-end: end,
                registration-deadline: reg-deadline,
                transfer-deadline: transfer-deadline
            }
        )
        (ok true)
    )
)


(define-map course-capacity
    {institution: principal, course-id: (string-ascii 20)}
    {
        max-students: uint,
        enrolled-students: uint,
        waitlist: (list 50 (string-ascii 20))
    }
)

(define-public (set-course-capacity 
    (course-id (string-ascii 20))
    (max-capacity uint))
    (begin
        (asserts! (is-registered tx-sender) err-not-registered)
        (map-set course-capacity {institution: tx-sender, course-id: course-id}
            {
                max-students: max-capacity,
                enrolled-students: u0,
                waitlist: (list)
            }
        )
        (ok true)
    )
)



(define-map course-certificates
    {institution: principal, student-id: (string-ascii 20), course-id: (string-ascii 20)}
    {
        completion-date: uint,
        grade: (string-ascii 2),
        certificate-hash: (string-ascii 64)
    }
)

(define-public (issue-certificate 
    (student-id (string-ascii 20))
    (course-id (string-ascii 20))
    (grade (string-ascii 2))
    (cert-hash (string-ascii 64)))
    (begin
        (asserts! (is-registered tx-sender) err-not-registered)
        (map-set course-certificates 
            {institution: tx-sender, student-id: student-id, course-id: course-id}
            {
                completion-date: stacks-block-height,
                grade: grade,
                certificate-hash: cert-hash
            }
        )
        (ok true)
    )
)


(define-map transfer-comments
    uint
    {
        comment: (string-ascii 200),
        timestamp: uint,
        author: principal
    }
)

(define-public (add-transfer-comment 
    (transfer-id uint)
    (comment (string-ascii 200)))
    (let
        ((transfer (unwrap! (map-get? credit-transfers transfer-id) err-transfer-not-found)))
        (asserts! (or 
            (is-eq tx-sender (get from-institution transfer))
            (is-eq tx-sender (get to-institution transfer))) 
            err-unauthorized)
        (map-set transfer-comments transfer-id
            {
                comment: comment,
                timestamp: stacks-block-height,
                author: tx-sender
            }
        )
        (ok true)
    )
)



(define-map achievement-badges
    {badge-id: (string-ascii 20)}
    {
        institution: principal,
        name: (string-ascii 50),
        description: (string-ascii 200),
        criteria: (string-ascii 100)
    }
)

(define-map student-badges
    {student-id: (string-ascii 20), badge-id: (string-ascii 20)}
    {
        issue-date: uint,
        issuer: principal,
        metadata-uri: (string-ascii 100)
    }
)

(define-public (create-badge 
    (badge-id (string-ascii 20))
    (name (string-ascii 50))
    (description (string-ascii 200))
    (criteria (string-ascii 100)))
    (begin
        (asserts! (is-registered tx-sender) err-not-registered)
        (map-set achievement-badges {badge-id: badge-id}
            {
                institution: tx-sender,
                name: name,
                description: description,
                criteria: criteria
            }
        )
        (ok true)
    )
)

(define-public (award-badge
    (student-id (string-ascii 20))
    (badge-id (string-ascii 20))
    (metadata-uri (string-ascii 100)))
    (begin
        (asserts! (is-registered tx-sender) err-not-registered)
        (map-set student-badges 
            {student-id: student-id, badge-id: badge-id}
            {
                issue-date: stacks-block-height,
                issuer: tx-sender,
                metadata-uri: metadata-uri
            }
        )
        (ok true)
    )
)


(define-map transfer-rules
    principal
    {
        min-credits: uint,
        max-credits: uint,
        expiration-blocks: uint,
        required-grade: (string-ascii 2),
        institution-whitelist: (list 50 principal)
    }
)

(define-public (set-transfer-rules
    (min-credits uint)
    (max-credits uint)
    (expiration-blocks uint)
    (required-grade (string-ascii 2))
    (whitelist (list 50 principal)))
    (begin
        (asserts! (is-registered tx-sender) err-not-registered)
        (map-set transfer-rules tx-sender
            {
                min-credits: min-credits,
                max-credits: max-credits,
                expiration-blocks: expiration-blocks,
                required-grade: required-grade,
                institution-whitelist: whitelist
            }
        )
        (ok true)
    )
)

(define-read-only (validate-transfer
    (from-institution principal)
    (credits uint)
    (completion-block uint)
    (grade (string-ascii 2)))
    (let
        ((rules (unwrap! (map-get? transfer-rules tx-sender) err-not-registered)))
        (ok (and
            (>= credits (get min-credits rules))
            (<= credits (get max-credits rules))
            (<= (- stacks-block-height completion-block) (get expiration-blocks rules))
            (is-eq grade (get required-grade rules))
            (is-some (index-of (get institution-whitelist rules) from-institution))
        ))
    )
)


(define-public (set-subscription-fee (fee uint))
    (begin
        (asserts! (is-owner) err-owner-only)
        (var-set subscription-fee fee)
        (ok true)
    )
)


(define-constant err-equivalency-exists (err u109))
(define-constant err-equivalency-not-found (err u110))
(define-constant err-invalid-equivalency (err u111))

(define-map course-equivalencies
    {
        from-institution: principal,
        from-course-id: (string-ascii 20),
        to-institution: principal,
        to-course-id: (string-ascii 20)
    }
    {
        credit-ratio: uint,
        approved-by-from: bool,
        approved-by-to: bool,
        creation-date: uint,
        expiry-date: uint
    }
)

(define-map equivalency-proposals
    uint
    {
        from-institution: principal,
        from-course-id: (string-ascii 20),
        to-institution: principal,
        to-course-id: (string-ascii 20),
        credit-ratio: uint,
        proposer: principal,
        status: (string-ascii 10),
        creation-date: uint
    }
)

(define-data-var equivalency-nonce uint u0)

(define-public (propose-course-equivalency
    (from-institution principal)
    (from-course-id (string-ascii 20))
    (to-institution principal)
    (to-course-id (string-ascii 20))
    (credit-ratio uint))
    (let
        ((proposal-id (var-get equivalency-nonce)))
        (asserts! (is-registered tx-sender) err-not-registered)
        (asserts! (or (is-eq tx-sender from-institution) (is-eq tx-sender to-institution)) err-unauthorized)
        (asserts! (> credit-ratio u0) err-invalid-equivalency)
        (asserts! (is-some (map-get? courses {institution: from-institution, course-id: from-course-id})) err-course-not-found)
        (asserts! (is-some (map-get? courses {institution: to-institution, course-id: to-course-id})) err-course-not-found)
        
        (map-set equivalency-proposals proposal-id
            {
                from-institution: from-institution,
                from-course-id: from-course-id,
                to-institution: to-institution,
                to-course-id: to-course-id,
                credit-ratio: credit-ratio,
                proposer: tx-sender,
                status: "pending",
                creation-date: stacks-block-height
            }
        )
        (var-set equivalency-nonce (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (approve-equivalency-proposal (proposal-id uint))
    (let
        ((proposal (unwrap! (map-get? equivalency-proposals proposal-id) err-equivalency-not-found)))
        (asserts! (or 
            (is-eq tx-sender (get from-institution proposal))
            (is-eq tx-sender (get to-institution proposal))) err-unauthorized)
        (asserts! (is-eq (get status proposal) "pending") err-invalid-equivalency)
        
        (map-set equivalency-proposals proposal-id
            (merge proposal {status: "approved"})
        )
        
        (map-set course-equivalencies
            {
                from-institution: (get from-institution proposal),
                from-course-id: (get from-course-id proposal),
                to-institution: (get to-institution proposal),
                to-course-id: (get to-course-id proposal)
            }
            {
                credit-ratio: (get credit-ratio proposal),
                approved-by-from: (is-eq tx-sender (get from-institution proposal)),
                approved-by-to: (is-eq tx-sender (get to-institution proposal)),
                creation-date: stacks-block-height,
                expiry-date: (+ stacks-block-height u52560)
            }
        )
        (ok true)
    )
)

(define-public (initiate-auto-credit-transfer
    (to-institution principal)
    (student-id (string-ascii 20))
    (course-id (string-ascii 20))
    (target-course-id (string-ascii 20)))
    (let
        ((course (unwrap! (map-get? courses {institution: tx-sender, course-id: course-id}) err-course-not-found))
         (equivalency (unwrap! (map-get? course-equivalencies 
            {
                from-institution: tx-sender,
                from-course-id: course-id,
                to-institution: to-institution,
                to-course-id: target-course-id
            }) err-equivalency-not-found))
         (nonce (var-get transfer-nonce))
         (adjusted-credits (/ (* (get credits course) (get credit-ratio equivalency)) u100)))
        
        (asserts! (is-registered tx-sender) err-not-registered)
        (asserts! (is-registered to-institution) err-not-registered)
        (asserts! (get verified course) err-unauthorized)
        (asserts! (and (get approved-by-from equivalency) (get approved-by-to equivalency)) err-unauthorized)
        (asserts! (< stacks-block-height (get expiry-date equivalency)) err-invalid-equivalency)
        
        ;; (map-set credit-transfers nonce
        ;;     {
        ;;         from-institution: tx-sender,
        ;;         to-institution: to-institution,
        ;;         student-id: student-id,
        ;;         course-id: target-course-id,
        ;;         credits: adjusted-credits,
        ;;         status: "auto-approved",
        ;;         timestamp: stacks-block-height
        ;;     }
        ;; )
        (var-set transfer-nonce (+ nonce u1))
        (ok nonce)
    )
)

(define-read-only (get-course-equivalency
    (from-institution principal)
    (from-course-id (string-ascii 20))
    (to-institution principal)
    (to-course-id (string-ascii 20)))
    (map-get? course-equivalencies
        {
            from-institution: from-institution,
            from-course-id: from-course-id,
            to-institution: to-institution,
            to-course-id: to-course-id
        }
    )
)

(define-read-only (get-equivalency-proposal (proposal-id uint))
    (map-get? equivalency-proposals proposal-id)
)

(define-read-only (check-auto-transfer-eligibility
    (from-institution principal)
    (from-course-id (string-ascii 20))
    (to-institution principal)
    (to-course-id (string-ascii 20)))
    (match (map-get? course-equivalencies
        {
            from-institution: from-institution,
            from-course-id: from-course-id,
            to-institution: to-institution,
            to-course-id: to-course-id
        })
        equivalency (ok (and 
            (get approved-by-from equivalency)
            (get approved-by-to equivalency)
            (< stacks-block-height (get expiry-date equivalency))))
        (ok false)
    )
)

;; Read-only functions for portfolio system
(define-read-only (get-degree-template (institution principal) (degree-id (string-ascii 30)))
    (map-get? degree-templates {institution: institution, degree-id: degree-id})
)

(define-read-only (get-student-portfolio (student-id (string-ascii 20)))
    (map-get? student-portfolios student-id)
)

(define-read-only (get-portfolio-credit (student-id (string-ascii 20)) (credit-id uint))
    (map-get? portfolio-credits {student-id: student-id, credit-id: credit-id})
)

(define-read-only (get-degree-progress 
    (student-id (string-ascii 20))
    (institution principal)
    (degree-id (string-ascii 30)))
    (map-get? degree-progress {student-id: student-id, institution: institution, degree-id: degree-id})
)

;; Institutional Reputation System Constants
(define-constant err-invalid-reputation-score (err u112))
(define-constant err-reputation-calculation-error (err u113))
(define-constant err-insufficient-data (err u114))
(define-constant min-reputation-score u0)
(define-constant max-reputation-score u1000)
(define-constant reputation-decay-rate u5)
(define-constant min-transfers-for-score u3)

;; Reputation Data Maps
(define-map institutional-reputation
    principal
    {
        overall-score: uint,
        transfer-success-rate: uint,
        course-quality-score: uint,
        response-time-score: uint,
        total-transfers: uint,
        successful-transfers: uint,
        last-updated: uint,
        reputation-tier: (string-ascii 10)
    }
)

(define-map reputation-metrics
    principal
    {
        pending-transfers: uint,
        rejected-transfers: uint,
        average-response-time: uint,
        total-course-ratings: uint,
        total-rating-points: uint,
        verification-requests: uint,
        successful-verifications: uint
    }
)

(define-map reputation-history
    {institution: principal, timestamp: uint}
    {
        score: uint,
        reason: (string-ascii 50),
        change: int
    }
)

(define-data-var reputation-history-nonce uint u0)

;; Private Functions
(define-private (calculate-transfer-success-rate (institution principal))
    (let
        ((reputation-data (default-to 
            {
                overall-score: u500,
                transfer-success-rate: u0,
                course-quality-score: u0,
                response-time-score: u0,
                total-transfers: u0,
                successful-transfers: u0,
                last-updated: u0,
                reputation-tier: "unrated"
            }
            (map-get? institutional-reputation institution))))
        (if (> (get total-transfers reputation-data) u0)
            (/ (* (get successful-transfers reputation-data) u100) (get total-transfers reputation-data))
            u0
        )
    )
)

(define-private (calculate-course-quality-score (institution principal))
    (let
        ((metrics (default-to
            {
                pending-transfers: u0,
                rejected-transfers: u0,
                average-response-time: u0,
                total-course-ratings: u0,
                total-rating-points: u0,
                verification-requests: u0,
                successful-verifications: u0
            }
            (map-get? reputation-metrics institution))))
        (if (> (get total-course-ratings metrics) u0)
            (/ (* (get total-rating-points metrics) u200) (get total-course-ratings metrics))
            u0
        )
    )
)

(define-private (calculate-response-time-score (average-response-time uint))
    (if (<= average-response-time u144)
        u1000
        (if (<= average-response-time u1008)
            u750
            (if (<= average-response-time u4032)
                u500
                u250
            )
        )
    )
)

(define-private (determine-reputation-tier (score uint))
    (if (>= score u900)
        "platinum"
        (if (>= score u750)
            "gold"
            (if (>= score u600)
                "silver"
                (if (>= score u400)
                    "bronze"
                    "basic"
                )
            )
        )
    )
)

(define-private (calculate-overall-score 
    (transfer-rate uint)
    (quality-score uint)
    (response-score uint))
    (let
        ((weighted-transfer (* transfer-rate u4))
         (weighted-quality (* quality-score u3))
         (weighted-response (* response-score u3)))
        (/ (+ weighted-transfer weighted-quality weighted-response) u10)
    )
)

(define-private (record-reputation-change 
    (institution principal)
    (score uint)
    (reason (string-ascii 50))
    (change int))
    (let
        ((history-id (var-get reputation-history-nonce)))
        (map-set reputation-history
            {institution: institution, timestamp: stacks-block-height}
            {
                score: score,
                reason: reason,
                change: change
            }
        )
        (var-set reputation-history-nonce (+ history-id u1))
        true
    )
)

;; Public Functions
(define-public (initialize-institution-reputation (institution principal))
    (begin
        (asserts! (is-registered institution) err-not-registered)
        (asserts! (is-none (map-get? institutional-reputation institution)) err-already-registered)
        (map-set institutional-reputation institution
            {
                overall-score: u500,
                transfer-success-rate: u0,
                course-quality-score: u0,
                response-time-score: u500,
                total-transfers: u0,
                successful-transfers: u0,
                last-updated: stacks-block-height,
                reputation-tier: "basic"
            }
        )
        (map-set reputation-metrics institution
            {
                pending-transfers: u0,
                rejected-transfers: u0,
                average-response-time: u0,
                total-course-ratings: u0,
                total-rating-points: u0,
                verification-requests: u0,
                successful-verifications: u0
            }
        )
        (ok true)
    )
)

(define-public (update-transfer-outcome 
    (institution principal)
    (successful bool)
    (response-time uint))
    (let
        ((current-reputation (unwrap! (map-get? institutional-reputation institution) err-not-registered))
         (current-metrics (unwrap! (map-get? reputation-metrics institution) err-not-registered))
         (new-total-transfers (+ (get total-transfers current-reputation) u1))
         (new-successful-transfers (if successful 
             (+ (get successful-transfers current-reputation) u1)
             (get successful-transfers current-reputation)))
         (new-avg-response-time (if (> (get total-transfers current-reputation) u0)
             (/ (+ (* (get average-response-time current-metrics) (get total-transfers current-reputation)) response-time) 
                new-total-transfers)
             response-time)))
        
        (map-set institutional-reputation institution
            (merge current-reputation
                {
                    total-transfers: new-total-transfers,
                    successful-transfers: new-successful-transfers,
                    last-updated: stacks-block-height
                }
            )
        )
        (map-set reputation-metrics institution
            (merge current-metrics
                {
                    average-response-time: new-avg-response-time,
                    pending-transfers: (if successful 
                        (if (> (get pending-transfers current-metrics) u0)
                            (- (get pending-transfers current-metrics) u1)
                            u0)
                        (get pending-transfers current-metrics)),
                    rejected-transfers: (if successful
                        (get rejected-transfers current-metrics)
                        (+ (get rejected-transfers current-metrics) u1))
                }
            )
        )
        (ok true)
    )
)

(define-public (update-course-rating-metrics
    (institution principal)
    (rating uint))
    (let
        ((current-metrics (unwrap! (map-get? reputation-metrics institution) err-not-registered)))
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
        (map-set reputation-metrics institution
            (merge current-metrics
                {
                    total-course-ratings: (+ (get total-course-ratings current-metrics) u1),
                    total-rating-points: (+ (get total-rating-points current-metrics) rating)
                }
            )
        )
        (ok true)
    )
)




