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


