;; Simple Sidewalk Scooter Share
;; Core scooter checkout and management system

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_SCOOTER_NOT_FOUND (err u101))
(define-constant ERR_SCOOTER_UNAVAILABLE (err u102))
(define-constant ERR_ALREADY_CHECKED_OUT (err u103))
(define-constant ERR_INVALID_LOCATION (err u104))

(define-map scooters
  { scooter-id: uint }
  {
    location: (string-ascii 50),
    available: bool,
    battery-level: uint,
    last-maintenance: uint
  }
)

(define-map active-rentals
  { user: principal }
  {
    scooter-id: uint,
    start-time: uint,
    start-location: (string-ascii 50)
  }
)

(define-map helmets
  { helmet-id: uint }
  {
    location: (string-ascii 50),
    available: bool,
    size: (string-ascii 10)
  }
)

(define-data-var next-scooter-id uint u1)
(define-data-var next-helmet-id uint u1)

(define-public (add-scooter (location (string-ascii 50)) (battery-level uint))
  (let ((scooter-id (var-get next-scooter-id)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set scooters
      { scooter-id: scooter-id }
      {
        location: location,
        available: true,
        battery-level: battery-level,
        last-maintenance: stacks-block-height
      }
    )
    (var-set next-scooter-id (+ scooter-id u1))
    (ok scooter-id)
  )
)

(define-public (add-helmet (location (string-ascii 50)) (size (string-ascii 10)))
  (let ((helmet-id (var-get next-helmet-id)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set helmets
      { helmet-id: helmet-id }
      {
        location: location,
        available: true,
        size: size
      }
    )
    (var-set next-helmet-id (+ helmet-id u1))
    (ok helmet-id)
  )
)

(define-public (checkout-scooter (scooter-id uint) (location (string-ascii 50)))
  (let ((scooter (unwrap! (map-get? scooters { scooter-id: scooter-id }) ERR_SCOOTER_NOT_FOUND)))
    (asserts! (get available scooter) ERR_SCOOTER_UNAVAILABLE)
    (asserts! (is-none (map-get? active-rentals { user: tx-sender })) ERR_ALREADY_CHECKED_OUT)

    (map-set scooters
      { scooter-id: scooter-id }
      (merge scooter { available: false })
    )

    (map-set active-rentals
      { user: tx-sender }
      {
        scooter-id: scooter-id,
        start-time: stacks-block-height,
        start-location: location
      }
    )
    (ok true)
  )
)

(define-public (return-scooter (location (string-ascii 50)))
  (let (
    (rental (unwrap! (map-get? active-rentals { user: tx-sender }) ERR_SCOOTER_NOT_FOUND))
    (scooter-id (get scooter-id rental))
    (scooter (unwrap! (map-get? scooters { scooter-id: scooter-id }) ERR_SCOOTER_NOT_FOUND))
  )
    (map-set scooters
      { scooter-id: scooter-id }
      (merge scooter {
        available: true,
        location: location
      })
    )

    (map-delete active-rentals { user: tx-sender })
    (ok true)
  )
)

(define-read-only (get-scooter-info (scooter-id uint))
  (map-get? scooters { scooter-id: scooter-id })
)

(define-read-only (get-user-rental (user principal))
  (map-get? active-rentals { user: user })
)

(define-read-only (get-helmet-info (helmet-id uint))
  (map-get? helmets { helmet-id: helmet-id })
)

(define-read-only (get-available-scooters-at-location (location (string-ascii 50)))
  (ok location)
)
