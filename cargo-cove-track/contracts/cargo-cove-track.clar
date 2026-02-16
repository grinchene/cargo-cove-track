;; CargoCove - Decentralized Supply Chain Verification Platform
;; A smart contract for tracking supply chain compliance and verification

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-tier (err u104))

;; Data Variables
(define-data-var platform-active bool true)
(define-data-var total-suppliers uint u0)
(define-data-var total-products uint u0)
(define-data-var total-verifications uint u0)
(define-data-var total-proposals uint u0)

;; Data Maps
(define-map suppliers
    { supplier-id: uint }
    {
        owner: principal,
        name: (string-ascii 100),
        compliance-score: uint,
        tier-1-verified: bool,
        tier-2-verified: bool,
        tier-3-verified: bool,
        active: bool,
        registered-at: uint
    }
)

(define-map products
    { product-id: uint }
    {
        supplier-id: uint,
        name: (string-ascii 100),
        carbon-footprint: uint,
        origin-verified: bool,
        qr-code-hash: (buff 32),
        created-at: uint
    }
)

(define-map verification-records
    { record-id: uint }
    {
        supplier-id: uint,
        tier: uint,
        verified-by: principal,
        compliance-data: (string-ascii 256),
        timestamp: uint,
        passed: bool
    }
)

(define-map governance-proposals
    { proposal-id: uint }
    {
        proposer: principal,
        description: (string-ascii 256),
        votes-for: uint,
        votes-against: uint,
        active: bool,
        created-at: uint
    }
)

(define-map stakeholder-votes
    { voter: principal, proposal-id: uint }
    { weight: uint, voted-for: bool }
)

;; Private Functions
(define-private (calculate-compliance-score (tier-1 bool) (tier-2 bool) (tier-3 bool))
    (+ 
        (if tier-1 u33 u0)
        (if tier-2 u33 u0)
        (if tier-3 u34 u0)
    )
)

;; Public Functions - Supplier Management
(define-public (register-supplier (name (string-ascii 100)))
    (let
        (
            (new-supplier-id (+ (var-get total-suppliers) u1))
        )
        (asserts! (var-get platform-active) (err u105))
        (map-set suppliers
            { supplier-id: new-supplier-id }
            {
                owner: tx-sender,
                name: name,
                compliance-score: u0,
                tier-1-verified: false,
                tier-2-verified: false,
                tier-3-verified: false,
                active: true,
                registered-at: block-height
            }
        )
        (var-set total-suppliers new-supplier-id)
        (ok new-supplier-id)
    )
)

(define-public (add-verification (supplier-id uint) (tier uint) (compliance-data (string-ascii 256)) (passed bool))
    (let
        (
            (supplier (unwrap! (map-get? suppliers { supplier-id: supplier-id }) err-not-found))
            (record-id (+ (var-get total-verifications) u1))
        )
        (asserts! (var-get platform-active) (err u105))
        (asserts! (or (is-eq tier u1) (is-eq tier u2) (is-eq tier u3)) err-invalid-tier)
        
        ;; Store verification record
        (map-set verification-records
            { record-id: record-id }
            {
                supplier-id: supplier-id,
                tier: tier,
                verified-by: tx-sender,
                compliance-data: compliance-data,
                timestamp: block-height,
                passed: passed
            }
        )
        
        ;; Update supplier verification status
        (map-set suppliers
            { supplier-id: supplier-id }
            (merge supplier {
                tier-1-verified: (if (is-eq tier u1) passed (get tier-1-verified supplier)),
                tier-2-verified: (if (is-eq tier u2) passed (get tier-2-verified supplier)),
                tier-3-verified: (if (is-eq tier u3) passed (get tier-3-verified supplier)),
                compliance-score: (calculate-compliance-score
                    (if (is-eq tier u1) passed (get tier-1-verified supplier))
                    (if (is-eq tier u2) passed (get tier-2-verified supplier))
                    (if (is-eq tier u3) passed (get tier-3-verified supplier))
                )
            })
        )
        
        (var-set total-verifications record-id)
        (ok record-id)
    )
)

;; Product Management
(define-public (register-product (supplier-id uint) (name (string-ascii 100)) (carbon-footprint uint) (qr-hash (buff 32)))
    (let
        (
            (supplier (unwrap! (map-get? suppliers { supplier-id: supplier-id }) err-not-found))
            (new-product-id (+ (var-get total-products) u1))
        )
        (asserts! (is-eq (get owner supplier) tx-sender) err-unauthorized)
        (asserts! (get active supplier) err-unauthorized)
        
        (map-set products
            { product-id: new-product-id }
            {
                supplier-id: supplier-id,
                name: name,
                carbon-footprint: carbon-footprint,
                origin-verified: (get tier-1-verified supplier),
                qr-code-hash: qr-hash,
                created-at: block-height
            }
        )
        (var-set total-products new-product-id)
        (ok new-product-id)
    )
)

;; Governance Functions
(define-public (create-proposal (description (string-ascii 256)))
    (let
        (
            (proposal-id (+ (var-get total-proposals) u1))
        )
        (map-set governance-proposals
            { proposal-id: proposal-id }
            {
                proposer: tx-sender,
                description: description,
                votes-for: u0,
                votes-against: u0,
                active: true,
                created-at: block-height
            }
        )
        (var-set total-proposals proposal-id)
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool) (weight uint))
    (let
        (
            (proposal (unwrap! (map-get? governance-proposals { proposal-id: proposal-id }) err-not-found))
        )
        (asserts! (get active proposal) (err u106))
        
        (map-set stakeholder-votes
            { voter: tx-sender, proposal-id: proposal-id }
            { weight: weight, voted-for: vote-for }
        )
        
        (map-set governance-proposals
            { proposal-id: proposal-id }
            (merge proposal {
                votes-for: (if vote-for (+ (get votes-for proposal) weight) (get votes-for proposal)),
                votes-against: (if vote-for (get votes-against proposal) (+ (get votes-against proposal) weight))
            })
        )
        (ok true)
    )
)

;; Read-only Functions
(define-read-only (get-supplier (supplier-id uint))
    (map-get? suppliers { supplier-id: supplier-id })
)

(define-read-only (get-product (product-id uint))
    (map-get? products { product-id: product-id })
)

(define-read-only (get-verification-record (record-id uint))
    (map-get? verification-records { record-id: record-id })
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? governance-proposals { proposal-id: proposal-id })
)

(define-read-only (get-compliance-score (supplier-id uint))
    (match (map-get? suppliers { supplier-id: supplier-id })
        supplier (ok (get compliance-score supplier))
        err-not-found
    )
)

(define-read-only (get-total-suppliers)
    (ok (var-get total-suppliers))
)

(define-read-only (get-total-products)
    (ok (var-get total-products))
)

(define-read-only (get-total-verifications)
    (ok (var-get total-verifications))
)

;; Admin Functions
(define-public (toggle-platform)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set platform-active (not (var-get platform-active)))
        (ok (var-get platform-active))
    )
)