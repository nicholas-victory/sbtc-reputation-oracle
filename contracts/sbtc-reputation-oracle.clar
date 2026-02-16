;; title: sbtc-reputation-oracle
;; version:
;; summary:
;; description:
;; Music Royalty Distribution and Rights Management System
;; This contract manages the transparent distribution of music royalties among artists, 
;; producers, and rights holders with secure tracking of payments and earnings.

;; Error constants
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-INVALID-SHARE-VALUE (err u101))
(define-constant ERR-TRACK-ALREADY-EXISTS (err u102))
(define-constant ERR-TRACK-NOT-FOUND (err u103))
(define-constant ERR-PAYMENT-SHORTAGE (err u104))
(define-constant ERR-INVALID-STAKEHOLDER (err u105))
(define-constant ERR-DISBURSEMENT-FAILED (err u106))
(define-constant ERR-INVALID-STRING-SIZE (err u107))
(define-constant ERR-INVALID-TRACK-NAME (err u108))
(define-constant ERR-INVALID-MEMBER-TYPE (err u109))
(define-constant ERR-INVALID-PERFORMER (err u110))
(define-constant ERR-INVALID-MANAGER (err u111))

;; Data structures
(define-map track-catalog
  { track-id: uint }
  {
    track-name: (string-ascii 50),
    performer: principal,
    lifetime-revenue: uint,
    registration-date: uint,
    distribution-enabled: bool,
  }
)

(define-map revenue-shares
  {
    track-id: uint,
    stakeholder: principal,
  }
  {
    percentage-share: uint,
    member-type: (string-ascii 20),
    earned-to-date: uint,
  }
)

;; State variables
(define-data-var catalog-size uint u0)
(define-data-var catalog-manager principal tx-sender)

;; Read-only functions - Data access
(define-read-only (get-track-info (track-id uint))
  (map-get? track-catalog { track-id: track-id })
)

(define-read-only (get-stakeholder-info
    (track-id uint)
    (stakeholder principal)
  )
  (map-get? revenue-shares {
    track-id: track-id,
    stakeholder: stakeholder,
  })
)

(define-read-only (get-catalog-size)
  (var-get catalog-size)
)

(define-read-only (get-track-stakeholders (track-id uint))
  (let (
      (track-info (get-track-info track-id))
      (main-performer (match track-info
        record (get performer record)
        tx-sender
      ))
    )
    (let ((stakeholder-data (get-stakeholder-info track-id main-performer)))
      (match stakeholder-data
        share (list {
          stakeholder: main-performer,
          percentage-share: (get percentage-share share),
        })
        (list)
      )
    )
  )
)

;; Private validation functions
(define-private (is-valid-share-data (share {
  percentage-share: uint,
  member-type: (string-ascii 20),
  earned-to-date: uint,
}))
  (> (get percentage-share share) u0)
)

(define-private (is-catalog-manager)
  (is-eq tx-sender (var-get catalog-manager))
)

(define-private (validate-percentage-range (percentage uint))
  (and (>= percentage u0) (<= percentage u100))
)

(define-private (validate-string-input (input (string-ascii 50)))
  (let ((string-length (len input)))
    (and (> string-length u0) (<= string-length u50))
  )
)

(define-private (validate-member-type (type-value (string-ascii 20)))
  (let ((type-length (len type-value)))
    (and (> type-length u0) (<= type-length u20))
  )
)

(define-private (validate-stakeholder-address (address principal))
  (and
    (not (is-eq address tx-sender))
    (not (is-eq address (var-get catalog-manager)))
  )
)

;; Private payment processing functions
(define-private (process-share-payment
    (allocation {
      stakeholder: principal,
      percentage-share: uint,
    })
    (payout-amount uint)
  )
  (let ((recipient-share (/ (* payout-amount (get percentage-share allocation)) u100)))
    (if (> recipient-share u0)
      (match (stx-transfer? recipient-share tx-sender (get stakeholder allocation))
        success payout-amount
        error u0
      )
      u0
    )
  )
)

(define-private (execute-payments
    (track-id uint)
    (payout-amount uint)
  )
  (let (
      (stakeholder-list (get-track-stakeholders track-id))
      (total-distributed (fold process-share-payment stakeholder-list payout-amount))
    )
    (begin
      (asserts! (> (len stakeholder-list) u0) ERR-TRACK-NOT-FOUND)
      (asserts! (> total-distributed u0) ERR-DISBURSEMENT-FAILED)
      (ok total-distributed)
    )
  )
)

;; Public functions - Administrative
(define-public (register-track
    (track-name (string-ascii 50))
    (performer principal)
  )
  (let ((new-track-id (+ (var-get catalog-size) u1)))
    (begin
      (asserts! (is-catalog-manager) ERR-UNAUTHORIZED-ACCESS)
      (asserts! (validate-string-input track-name) ERR-INVALID-TRACK-NAME)
      (asserts! (validate-stakeholder-address performer) ERR-INVALID-PERFORMER)

      (map-set track-catalog { track-id: new-track-id } {
        track-name: track-name,
        performer: performer,
        lifetime-revenue: u0,
        registration-date: stacks-block-height,
        distribution-enabled: true,
      })
      (var-set catalog-size new-track-id)
      (ok new-track-id)
    )
  )
)

(define-public (set-share-allocation
    (track-id uint)
    (stakeholder principal)
    (percentage-share uint)
    (member-type (string-ascii 20))
  )
  (let ((track-data (get-track-info track-id)))
    (begin
      (asserts! (is-some track-data) ERR-TRACK-NOT-FOUND)
      (asserts! (validate-percentage-range percentage-share)
        ERR-INVALID-SHARE-VALUE
      )
      (asserts! (validate-member-type member-type) ERR-INVALID-MEMBER-TYPE)
      (asserts! (validate-stakeholder-address stakeholder)
        ERR-INVALID-STAKEHOLDER
      )

      (map-set revenue-shares {
        track-id: track-id,
        stakeholder: stakeholder,
      } {
        percentage-share: percentage-share,
        member-type: member-type,
        earned-to-date: u0,
      })
      (ok true)
    )
  )
)

(define-public (set-track-status
    (track-id uint)
    (status-enabled bool)
  )
  (let ((track-data (get-track-info track-id)))
    (begin
      (asserts! (is-catalog-manager) ERR-UNAUTHORIZED-ACCESS)
      (asserts! (is-some track-data) ERR-TRACK-NOT-FOUND)

      (map-set track-catalog { track-id: track-id }
        (merge (unwrap-panic track-data) { distribution-enabled: status-enabled })
      )
      (ok true)
    )
  )
)

(define-public (assign-manager (new-manager principal))
  (begin
    (asserts! (is-catalog-manager) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (validate-stakeholder-address new-manager) ERR-INVALID-MANAGER)

    (var-set catalog-manager new-manager)
    (ok true)
  )
)

;; Public functions - Payment processing
(define-public (distribute-royalties
    (track-id uint)
    (royalty-amount uint)
  )
  (let ((track-data (get-track-info track-id)))
    (begin
      (asserts! (is-some track-data) ERR-TRACK-NOT-FOUND)
      (asserts! (>= (stx-get-balance tx-sender) royalty-amount)
        ERR-PAYMENT-SHORTAGE
      )

      (try! (execute-payments track-id royalty-amount))
      (map-set track-catalog { track-id: track-id }
        (merge (unwrap-panic track-data) { lifetime-revenue: (+ (get lifetime-revenue (unwrap-panic track-data)) royalty-amount) })
      )
      (ok true)
    )
  )
)

;; Contract initialization
(begin
  (var-set catalog-size u0)
)
