;; FairReef Impact - Decentralized Social Impact Platform
;; A smart contract for impact investing with autonomous outcome verification

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-invalid-status (err u106))
(define-constant err-milestone-not-ready (err u107))
(define-constant err-invalid-validator (err u108))

;; Data Variables
(define-data-var project-counter uint u0)
(define-data-var validator-counter uint u0)
(define-data-var min-validator-stake uint u1000000) ;; 1 STX in microSTX

;; Project Status
(define-constant status-proposed u0)
(define-constant status-active u1)
(define-constant status-completed u2)
(define-constant status-failed u3)

;; Data Maps

;; Projects
(define-map projects
    uint
    {
        creator: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        funding-goal: uint,
        funds-raised: uint,
        status: uint,
        impact-score: uint,
        created-at: uint
    }
)

;; Milestones for each project
(define-map milestones
    {project-id: uint, milestone-id: uint}
    {
        description: (string-ascii 200),
        funding-amount: uint,
        required-validations: uint,
        current-validations: uint,
        is-verified: bool,
        verification-deadline: uint
    }
)

;; Project milestone counter
(define-map project-milestone-count uint uint)

;; Investors in projects
(define-map investments
    {project-id: uint, investor: principal}
    {
        amount: uint,
        invested-at: uint
    }
)

;; Validators
(define-map validators
    principal
    {
        stake-amount: uint,
        reputation-score: uint,
        validations-count: uint,
        accurate-validations: uint,
        is-active: bool,
        registered-at: uint
    }
)

;; Milestone validations by validators
(define-map milestone-validations
    {project-id: uint, milestone-id: uint, validator: principal}
    {
        is-approved: bool,
        validated-at: uint,
        evidence-hash: (string-ascii 64)
    }
)

;; Project escrow balances
(define-map project-escrow uint uint)

;; Read-only functions

(define-read-only (get-project (project-id uint))
    (map-get? projects project-id)
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
    (map-get? milestones {project-id: project-id, milestone-id: milestone-id})
)

(define-read-only (get-validator (validator principal))
    (map-get? validators validator)
)

(define-read-only (get-investment (project-id uint) (investor principal))
    (map-get? investments {project-id: project-id, investor: investor})
)

(define-read-only (get-milestone-validation (project-id uint) (milestone-id uint) (validator principal))
    (map-get? milestone-validations {project-id: project-id, milestone-id: milestone-id, validator: validator})
)

(define-read-only (get-project-escrow (project-id uint))
    (default-to u0 (map-get? project-escrow project-id))
)

(define-read-only (get-project-count)
    (var-get project-counter)
)

(define-read-only (get-milestone-count (project-id uint))
    (default-to u0 (map-get? project-milestone-count project-id))
)

;; Public functions

;; Create a new impact project
(define-public (create-project (title (string-ascii 100)) (description (string-ascii 500)) (funding-goal uint))
    (let
        (
            (project-id (+ (var-get project-counter) u1))
        )
        (asserts! (> funding-goal u0) err-invalid-amount)
        (map-set projects project-id
            {
                creator: tx-sender,
                title: title,
                description: description,
                funding-goal: funding-goal,
                funds-raised: u0,
                status: status-proposed,
                impact-score: u0,
                created-at: block-height
            }
        )
        (map-set project-escrow project-id u0)
        (map-set project-milestone-count project-id u0)
        (var-set project-counter project-id)
        (ok project-id)
    )
)

;; Add milestone to a project
(define-public (add-milestone (project-id uint) (description (string-ascii 200)) (funding-amount uint) (required-validations uint) (verification-deadline uint))
    (let
        (
            (project (unwrap! (map-get? projects project-id) err-not-found))
            (milestone-id (+ (get-milestone-count project-id) u1))
        )
        (asserts! (is-eq (get creator project) tx-sender) err-unauthorized)
        (asserts! (> funding-amount u0) err-invalid-amount)
        (asserts! (> required-validations u0) err-invalid-amount)
        (map-set milestones {project-id: project-id, milestone-id: milestone-id}
            {
                description: description,
                funding-amount: funding-amount,
                required-validations: required-validations,
                current-validations: u0,
                is-verified: false,
                verification-deadline: verification-deadline
            }
        )
        (map-set project-milestone-count project-id milestone-id)
        (ok milestone-id)
    )
)

;; Invest in a project
(define-public (invest-in-project (project-id uint) (amount uint))
    (let
        (
            (project (unwrap! (map-get? projects project-id) err-not-found))
            (current-investment (default-to {amount: u0, invested-at: u0} 
                (map-get? investments {project-id: project-id, investor: tx-sender})))
            (current-escrow (get-project-escrow project-id))
        )
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (is-eq (get status project) status-proposed) err-invalid-status)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update investment record
        (map-set investments {project-id: project-id, investor: tx-sender}
            {
                amount: (+ (get amount current-investment) amount),
                invested-at: block-height
            }
        )
        
        ;; Update project funds
        (map-set projects project-id
            (merge project {funds-raised: (+ (get funds-raised project) amount)})
        )
        
        ;; Update escrow
        (map-set project-escrow project-id (+ current-escrow amount))
        
        ;; Activate project if funding goal is met
        (if (>= (+ (get funds-raised project) amount) (get funding-goal project))
            (map-set projects project-id
                (merge project {
                    funds-raised: (+ (get funds-raised project) amount),
                    status: status-active
                })
            )
            true
        )
        
        (ok true)
    )
)

;; Register as a validator
(define-public (register-validator (stake-amount uint))
    (begin
        (asserts! (>= stake-amount (var-get min-validator-stake)) err-invalid-amount)
        (asserts! (is-none (map-get? validators tx-sender)) err-already-exists)
        
        ;; Transfer stake to contract
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        
        (map-set validators tx-sender
            {
                stake-amount: stake-amount,
                reputation-score: u100,
                validations-count: u0,
                accurate-validations: u0,
                is-active: true,
                registered-at: block-height
            }
        )
        (ok true)
    )
)

;; Validate a milestone
(define-public (validate-milestone (project-id uint) (milestone-id uint) (is-approved bool) (evidence-hash (string-ascii 64)))
    (let
        (
            (validator-data (unwrap! (map-get? validators tx-sender) err-invalid-validator))
            (milestone (unwrap! (map-get? milestones {project-id: project-id, milestone-id: milestone-id}) err-not-found))
            (project (unwrap! (map-get? projects project-id) err-not-found))
        )
        (asserts! (get is-active validator-data) err-unauthorized)
        (asserts! (is-eq (get status project) status-active) err-invalid-status)
        (asserts! (not (get is-verified milestone)) err-invalid-status)
        (asserts! (is-none (map-get? milestone-validations {project-id: project-id, milestone-id: milestone-id, validator: tx-sender})) err-already-exists)
        
        ;; Record validation
        (map-set milestone-validations {project-id: project-id, milestone-id: milestone-id, validator: tx-sender}
            {
                is-approved: is-approved,
                validated-at: block-height,
                evidence-hash: evidence-hash
            }
        )
        
        ;; Update milestone validation count
        (let
            (
                (new-validation-count (+ (get current-validations milestone) u1))
            )
            (map-set milestones {project-id: project-id, milestone-id: milestone-id}
                (merge milestone {current-validations: new-validation-count})
            )
            
            ;; Update validator stats
            (map-set validators tx-sender
                (merge validator-data {validations-count: (+ (get validations-count validator-data) u1)})
            )
            
            ;; Check if milestone is verified
            (if (and (>= new-validation-count (get required-validations milestone)) is-approved)
                (complete-milestone project-id milestone-id)
                (ok false)
            )
        )
    )
)

;; Private function to complete milestone and release funds
(define-private (complete-milestone (project-id uint) (milestone-id uint))
    (let
        (
            (milestone (unwrap! (map-get? milestones {project-id: project-id, milestone-id: milestone-id}) err-not-found))
            (project (unwrap! (map-get? projects project-id) err-not-found))
            (current-escrow (get-project-escrow project-id))
            (funding-amount (get funding-amount milestone))
        )
        (asserts! (>= current-escrow funding-amount) err-insufficient-funds)
        
        ;; Mark milestone as verified
        (map-set milestones {project-id: project-id, milestone-id: milestone-id}
            (merge milestone {is-verified: true})
        )
        
        ;; Release funds to project creator
        (try! (as-contract (stx-transfer? funding-amount tx-sender (get creator project))))
        
        ;; Update escrow
        (map-set project-escrow project-id (- current-escrow funding-amount))
        
        ;; Update project impact score
        (map-set projects project-id
            (merge project {impact-score: (+ (get impact-score project) u10)})
        )
        
        (ok true)
    )
)

;; Update validator reputation (only contract owner)
(define-public (update-validator-reputation (validator principal) (accurate bool))
    (let
        (
            (validator-data (unwrap! (map-get? validators validator) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (if accurate
            (map-set validators validator
                (merge validator-data {
                    accurate-validations: (+ (get accurate-validations validator-data) u1),
                    reputation-score: (+ (get reputation-score validator-data) u5)
                })
            )
            (map-set validators validator
                (merge validator-data {
                    reputation-score: (if (> (get reputation-score validator-data) u5)
                        (- (get reputation-score validator-data) u5)
                        u0)
                })
            )
        )
        (ok true)
    )
)

;; Withdraw validator stake
(define-public (withdraw-validator-stake)
    (let
        (
            (validator-data (unwrap! (map-get? validators tx-sender) err-not-found))
            (stake-amount (get stake-amount validator-data))
        )
        (asserts! (get is-active validator-data) err-unauthorized)
        
        ;; Deactivate validator
        (map-set validators tx-sender
            (merge validator-data {is-active: false})
        )
        
        ;; Return stake
        (try! (as-contract (stx-transfer? stake-amount tx-sender tx-sender)))
        
        (ok true)
    )
)

;; Mark project as completed (only project creator)
(define-public (complete-project (project-id uint))
    (let
        (
            (project (unwrap! (map-get? projects project-id) err-not-found))
        )
        (asserts! (is-eq (get creator project) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status project) status-active) err-invalid-status)
        
        (map-set projects project-id
            (merge project {status: status-completed})
        )
        (ok true)
    )
)
