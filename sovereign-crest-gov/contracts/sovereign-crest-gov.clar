;; SovereignCrest - Conviction-Weighted Voting Escrow Contract
;; A governance platform implementing conviction voting with quadratic weighting

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-conviction (err u103))
(define-constant err-proposal-ended (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-unauthorized (err u106))

;; Minimum conviction time (in blocks)
(define-constant min-conviction-time u1440) ;; ~10 days

;; Data Variables
(define-data-var proposal-nonce uint u0)
(define-data-var conviction-multiplier uint u100) ;; Base multiplier (100 = 1x)

;; Data Maps

;; User token locks
(define-map token-locks
  { user: principal }
  {
    amount: uint,
    lock-height: uint,
    unlock-height: uint
  }
)

;; Proposals
(define-map proposals
  { proposal-id: uint }
  {
    creator: principal,
    title: (string-ascii 256),
    description: (string-utf8 1024),
    start-height: uint,
    end-height: uint,
    total-conviction: uint,
    executed: bool,
    vote-threshold: uint
  }
)

;; User votes on proposals
(define-map votes
  { proposal-id: uint, voter: principal }
  {
    conviction-allocated: uint,
    voting-power: uint,
    signal-start-height: uint,
    tokens-allocated: uint
  }
)

;; User conviction across all proposals
(define-map user-conviction-state
  { user: principal }
  {
    total-tokens-locked: uint,
    active-proposal-count: uint,
    conviction-points: uint
  }
)

;; Delegation registry
(define-map delegations
  { delegator: principal, domain: (string-ascii 64) }
  { delegate: principal }
)

;; Read-only functions

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-user-lock (user principal))
  (map-get? token-locks { user: user })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-user-conviction (user principal))
  (map-get? user-conviction-state { user: user })
)

(define-read-only (get-delegation (delegator principal) (domain (string-ascii 64)))
  (map-get? delegations { delegator: delegator, domain: domain })
)

;; Calculate quadratic conviction voting power
;; Formula: sqrt(tokens * conviction_time * active_days)
(define-read-only (calculate-voting-power 
  (tokens uint) 
  (lock-duration uint) 
  (signal-duration uint))
  (let
    (
      (base-product (* tokens lock-duration))
      (conviction-product (* base-product signal-duration))
      ;; Simplified square root approximation
      (power-score (/ (* conviction-product (var-get conviction-multiplier)) u10000))
    )
    power-score
  )
)

;; Public functions

;; Lock tokens for governance participation
(define-public (lock-tokens (amount uint) (duration uint))
  (let
    (
      (caller tx-sender)
      (current-height block-height)
      (unlock-height (+ current-height duration))
    )
    (asserts! (>= duration min-conviction-time) err-invalid-conviction)
    
    ;; Store lock information
    (map-set token-locks
      { user: caller }
      {
        amount: amount,
        lock-height: current-height,
        unlock-height: unlock-height
      }
    )
    
    ;; Initialize conviction state
    (map-set user-conviction-state
      { user: caller }
      {
        total-tokens-locked: amount,
        active-proposal-count: u0,
        conviction-points: u0
      }
    )
    
    (ok true)
  )
)

;; Create a new proposal
(define-public (create-proposal 
  (title (string-ascii 256))
  (description (string-utf8 1024))
  (voting-period uint)
  (threshold uint))
  (let
    (
      (proposal-id (+ (var-get proposal-nonce) u1))
      (start-height block-height)
      (end-height (+ block-height voting-period))
    )
    (map-set proposals
      { proposal-id: proposal-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        start-height: start-height,
        end-height: end-height,
        total-conviction: u0,
        executed: false,
        vote-threshold: threshold
      }
    )
    
    (var-set proposal-nonce proposal-id)
    (ok proposal-id)
  )
)

;; Vote on a proposal with conviction allocation
(define-public (vote-with-conviction 
  (proposal-id uint) 
  (tokens-allocated uint))
  (let
    (
      (caller tx-sender)
      (proposal (unwrap! (get-proposal proposal-id) err-not-found))
      (user-lock (unwrap! (get-user-lock caller) err-insufficient-balance))
      (lock-duration (- (get unlock-height user-lock) (get lock-height user-lock)))
      (signal-duration (- block-height (get start-height proposal)))
      (voting-power (calculate-voting-power 
        tokens-allocated 
        lock-duration 
        signal-duration))
    )
    ;; Verify proposal is active
    (asserts! (< block-height (get end-height proposal)) err-proposal-ended)
    
    ;; Verify user hasn't voted
    (asserts! (is-none (get-vote proposal-id caller)) err-already-voted)
    
    ;; Verify sufficient locked tokens
    (asserts! (<= tokens-allocated (get amount user-lock)) err-insufficient-balance)
    
    ;; Record vote
    (map-set votes
      { proposal-id: proposal-id, voter: caller }
      {
        conviction-allocated: tokens-allocated,
        voting-power: voting-power,
        signal-start-height: block-height,
        tokens-allocated: tokens-allocated
      }
    )
    
    ;; Update proposal total conviction
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { total-conviction: (+ (get total-conviction proposal) voting-power) })
    )
    
    ;; Update user conviction state
    (match (get-user-conviction caller)
      user-state
        (map-set user-conviction-state
          { user: caller }
          (merge user-state {
            active-proposal-count: (+ (get active-proposal-count user-state) u1),
            conviction-points: (+ (get conviction-points user-state) voting-power)
          })
        )
      ;; Initialize if not found
      (map-set user-conviction-state
        { user: caller }
        {
          total-tokens-locked: tokens-allocated,
          active-proposal-count: u1,
          conviction-points: voting-power
        }
      )
    )
    
    (ok voting-power)
  )
)

;; Delegate voting power to another address for specific domain
(define-public (delegate-votes 
  (delegate principal) 
  (domain (string-ascii 64)))
  (begin
    (map-set delegations
      { delegator: tx-sender, domain: domain }
      { delegate: delegate }
    )
    (ok true)
  )
)

;; Execute proposal if threshold is met
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) err-not-found))
    )
    ;; Verify voting period has ended
    (asserts! (>= block-height (get end-height proposal)) err-proposal-ended)
    
    ;; Verify proposal hasn't been executed
    (asserts! (not (get executed proposal)) err-unauthorized)
    
    ;; Verify threshold is met
    (asserts! (>= (get total-conviction proposal) (get vote-threshold proposal)) err-unauthorized)
    
    ;; Mark as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )
    
    (ok true)
  )
)

;; Unlock tokens after lock period
(define-public (unlock-tokens)
  (let
    (
      (caller tx-sender)
      (user-lock (unwrap! (get-user-lock caller) err-not-found))
    )
    ;; Verify lock period has ended
    (asserts! (>= block-height (get unlock-height user-lock)) err-unauthorized)
    
    ;; Remove lock
    (map-delete token-locks { user: caller })
    
    (ok (get amount user-lock))
  )
)

;; Admin function to update conviction multiplier
(define-public (set-conviction-multiplier (new-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set conviction-multiplier new-multiplier)
    (ok true)
  )
)