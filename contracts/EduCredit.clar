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
