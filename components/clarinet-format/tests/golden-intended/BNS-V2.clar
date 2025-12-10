;; title: BNS-V2
;; version: V-2
;; summary: Updated BNS contract, handles the creation of new namespaces and new names on each namespace

;; traits
;; (new) Import SIP-09 NFT trait
(impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)
;; (new) Import a custom commission trait for handling commissions for NFT marketplaces functions
(use-trait commission-trait .commission-trait.commission)

;; token definition
;; (new) Define the non-fungible token (NFT) called BNS-V2 with unique identifiers as unsigned integers
(define-non-fungible-token BNS-V2 uint)
;; Time-to-live (TTL) constants for namespace preorders and name preorders, and the duration for name grace period.
;; The TTL for namespace and names preorders. (1 day)
(define-constant PREORDER-CLAIMABILITY-TTL u144)
;; The duration after revealing a namespace within which it must be launched. (1 year)
(define-constant NAMESPACE-LAUNCHABILITY-TTL u52595)
;; The grace period duration for name renewals post-expiration. (34 days)
(define-constant NAME-GRACE-PERIOD-DURATION u5000)
;; (new) The length of the hash should match this
(define-constant HASH160LEN u20)
;; Defines the price tiers for namespaces based on their lengths.
;; @format-ignore
(define-constant NAMESPACE-PRICE-TIERS (list
    u640000000000
    u64000000000 u64000000000
    u6400000000 u6400000000 u6400000000 u6400000000
    u640000000 u640000000 u640000000 u640000000 u640000000 u640000000 u640000000 u640000000 u640000000 u640000000 u640000000 u640000000 u640000000)
)

;; Only authorized caller to flip the switch and update URI
(define-constant DEPLOYER tx-sender)

;; (new) Var to store the token URI, allowing for metadata association with the NFT
(define-data-var token-uri (string-ascii 256) "ipfs://QmUQY1aZ799SPRaNBFqeCvvmZ4fTQfZvWHauRvHAukyQDB")

(define-public (update-token-uri (new-token-uri (string-ascii 256)))
    (ok (begin
        (asserts! (is-eq contract-caller DEPLOYER) ERR-NOT-AUTHORIZED)
        (var-set token-uri new-token-uri)
    ))
)

(define-data-var contract-uri (string-ascii 256) "ipfs://QmWKTZEMQNWngp23i7bgPzkineYC9LDvcxYkwNyVQVoH8y")

(define-public (update-contract-uri (new-contract-uri (string-ascii 256)))
    (ok (begin
        (asserts! (is-eq contract-caller DEPLOYER) ERR-NOT-AUTHORIZED)
        (var-set token-uri new-contract-uri)
    ))
)

;; errors
(define-constant ERR-UNWRAP (err u101))
(define-constant ERR-NOT-AUTHORIZED (err u102))
(define-constant ERR-NOT-LISTED (err u103))
(define-constant ERR-WRONG-COMMISSION (err u104))
(define-constant ERR-LISTED (err u105))
(define-constant ERR-NO-NAME (err u106))
(define-constant ERR-HASH-MALFORMED (err u107))
(define-constant ERR-STX-BURNT-INSUFFICIENT (err u108))
(define-constant ERR-PREORDER-NOT-FOUND (err u109))
(define-constant ERR-CHARSET-INVALID (err u110))
(define-constant ERR-NAMESPACE-ALREADY-EXISTS (err u111))
(define-constant ERR-PREORDER-CLAIMABILITY-EXPIRED (err u112))
(define-constant ERR-NAMESPACE-NOT-FOUND (err u113))
(define-constant ERR-OPERATION-UNAUTHORIZED (err u114))
(define-constant ERR-NAMESPACE-ALREADY-LAUNCHED (err u115))
(define-constant ERR-NAMESPACE-PREORDER-LAUNCHABILITY-EXPIRED (err u116))
(define-constant ERR-NAMESPACE-NOT-LAUNCHED (err u117))
(define-constant ERR-NAME-NOT-AVAILABLE (err u118))
(define-constant ERR-NAMESPACE-BLANK (err u119))
(define-constant ERR-NAME-BLANK (err u120))
(define-constant ERR-NAME-PREORDERED-BEFORE-NAMESPACE-LAUNCH (err u121))
(define-constant ERR-NAMESPACE-HAS-MANAGER (err u122))
(define-constant ERR-OVERFLOW (err u123))
(define-constant ERR-NO-NAMESPACE-MANAGER (err u124))
(define-constant ERR-FAST-MINTED-BEFORE (err u125))
(define-constant ERR-PREORDERED-BEFORE (err u126))
(define-constant ERR-NAME-NOT-CLAIMABLE-YET (err u127))
(define-constant ERR-IMPORTED-BEFORE (err u128))
(define-constant ERR-LIFETIME-EQUAL-0 (err u129))
(define-constant ERR-MIGRATION-IN-PROGRESS (err u130))
(define-constant ERR-NO-PRIMARY-NAME (err u131))

;; variables
;; (new) Variable to see if migration is complete
(define-data-var migration-complete bool false)

;; (new) Counter to keep track of the last minted NFT ID, ensuring unique identifiers
(define-data-var bns-index uint u0)

;; maps
;; (new) Map to track market listings, associating NFT IDs with price and commission details
(define-map market
    uint
    {
        price: uint,
        commission: principal,
    }
)

;; (new) Define a map to link NFT IDs to their respective names and namespaces.
(define-map index-to-name
    uint
    {
        name: (buff 48),
        namespace: (buff 20),
    }
)
;; (new) Define a map to link names and namespaces to their respective NFT IDs.
(define-map name-to-index
    {
        name: (buff 48),
        namespace: (buff 20),
    }
    uint
)

;; (updated) Contains detailed properties of names, including registration and importation times
(define-map name-properties
    {
        name: (buff 48),
        namespace: (buff 20),
    }
    {
        registered-at: (optional uint),
        imported-at: (optional uint),
        ;; The fqn used to make the earliest preorder at any given point
        hashed-salted-fqn-preorder: (optional (buff 20)),
        ;; Added this field in name-properties to know exactly who has the earliest preorder at any given point
        preordered-by: (optional principal),
        renewal-height: uint,
        stx-burn: uint,
        owner: principal,
    }
)

;; (update) Stores properties of namespaces, including their import principals, reveal and launch times, and pricing functions.
(define-map namespaces
    (buff 20)
    {
        namespace-manager: (optional principal),
        manager-transferable: bool,
        manager-frozen: bool,
        namespace-import: principal,
        revealed-at: uint,
        launched-at: (optional uint),
        lifetime: uint,
        can-update-price-function: bool,
        price-function: {
            buckets: (list 16 uint),
            base: uint,
            coeff: uint,
            nonalpha-discount: uint,
            no-vowel-discount: uint,
        },
    }
)

;; Records namespace preorder transactions with their creation times, and STX burned.
(define-map namespace-preorders
    {
        hashed-salted-namespace: (buff 20),
        buyer: principal,
    }
    {
        created-at: uint,
        stx-burned: uint,
        claimed: bool,
    }
)

;; Tracks preorders, to avoid attacks
(define-map namespace-single-preorder
    (buff 20)
    bool
)

;; Tracks preorders, to avoid attacks
(define-map name-single-preorder
    (buff 20)
    bool
)

;; Tracks preorders for names, including their creation times, and STX burned.
(define-map name-preorders
    {
        hashed-salted-fqn: (buff 20),
        buyer: principal,
    }
    {
        created-at: uint,
        stx-burned: uint,
        claimed: bool,
    }
)

;; It maps a user's principal to the ID of their primary name.
(define-map primary-name
    principal
    uint
)

;; read-only
;; @desc (new) SIP-09 compliant function to get the last minted token's ID
(define-read-only (get-last-token-id)
    ;; Returns the current value of bns-index variable, which tracks the last token ID
    (ok (var-get bns-index))
)

(define-read-only (get-renewal-height (id uint))
    (let (
            (name-namespace (unwrap! (get-bns-from-id id) ERR-NO-NAME))
            (namespace-props (unwrap! (map-get? namespaces (get namespace name-namespace))
                ERR-NAMESPACE-NOT-FOUND
            ))
            (name-props (unwrap! (map-get? name-properties name-namespace) ERR-NO-NAME))
            (renewal-height (get renewal-height name-props))
            (namespace-lifetime (get lifetime namespace-props))
        )
        ;; Check if the namespace requires renewals
        (asserts! (not (is-eq namespace-lifetime u0)) ERR-LIFETIME-EQUAL-0)
        ;; If the check passes then check the renewal-height of the name
        (ok (if (is-eq renewal-height u0)
            ;; If it is true then it means it was imported so return the namespace launch blockheight + lifetime
            (+
                (unwrap! (get launched-at namespace-props)
                    ERR-NAMESPACE-NOT-LAUNCHED
                )
                namespace-lifetime
            )
            renewal-height
        ))
    )
)

(define-read-only (can-resolve-name
        (namespace (buff 20))
        (name (buff 48))
    )
    (let (
            (name-id (unwrap! (get-id-from-bns name namespace) ERR-NO-NAME))
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
            (name-props (unwrap!
                (map-get? name-properties {
                    name: name,
                    namespace: namespace,
                })
                ERR-NO-NAME
            ))
            (renewal-height (get renewal-height name-props))
            (namespace-lifetime (get lifetime namespace-props))
        )
        ;; Check if the name can resolve
        (ok (if (is-eq u0 namespace-lifetime)
            ;; If true it means that the name is in a managed namespace or the namespace does not require renewals
            {
                renewal: u0,
                owner: (get owner name-props),
            }
            ;; If false then calculate renewal-height
            {
                renewal: (try! (get-renewal-height name-id)),
                owner: (get owner name-props),
            }
        ))
    )
)

;; @desc (new) SIP-09 compliant function to get token URI
(define-read-only (get-token-uri (id uint))
    ;; Returns a predefined set URI for the token metadata
    (ok (some (var-get token-uri)))
)

(define-read-only (get-contract-uri)
    ;; Returns a predefined set URI for the contract metadata
    (ok (some (var-get contract-uri)))
)

;; @desc (new) SIP-09 compliant function to get the owner of a specific token by its ID
(define-read-only (get-owner (id uint))
    ;; Check and return the owner of the specified NFT
    (ok (nft-get-owner? BNS-V2 id))
)

;; @desc (new) New get owner function
(define-read-only (get-owner-name
        (name (buff 48))
        (namespace (buff 20))
    )
    ;; Check and return the owner of the specified NFT
    (ok (nft-get-owner? BNS-V2 (unwrap! (get-id-from-bns name namespace) ERR-NO-NAME)))
)

;; Read-only function `get-namespace-price` calculates the registration price for a namespace based on its length.
;; @params:
;; namespace (buff 20): The namespace for which the price is being calculated.
(define-read-only (get-namespace-price (namespace (buff 20)))
    (let (
            ;; Calculate the length of the namespace.
            (namespace-len (len namespace))
        )
        ;; Ensure the namespace is not blank, its length is greater than 0.
        (asserts! (> namespace-len u0) ERR-NAMESPACE-BLANK)
        ;; Retrieve the price for the namespace based on its length from the NAMESPACE-PRICE-TIERS list.
        ;; The price tier is determined by the minimum of 7 or the namespace length minus one.
        (ok (unwrap!
            (element-at? NAMESPACE-PRICE-TIERS (min u7 (- namespace-len u1)))
            ERR-UNWRAP
        ))
    )
)

;; Read-only function `get-name-price` calculates the registration price for a name based on the price buckets of the namespace
;; @params:
;; namespace (buff 20): The namespace for which the price is being calculated.
;; name (buff 48): The name for which the price is being calculated.
(define-read-only (get-name-price
        (namespace (buff 20))
        (name (buff 48))
    )
    (let ((namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND)))
        (ok (compute-name-price name (get price-function namespace-props)))
    )
)

;; Read-only function `can-namespace-be-registered` checks if a namespace is available for registration.
;; @params:
;; namespace (buff 20): The namespace being checked for availability.
(define-read-only (can-namespace-be-registered (namespace (buff 20)))
    ;; Returns the result of `is-namespace-available` directly, indicating if the namespace can be registered.
    (ok (is-namespace-available namespace))
)

;; Read-only function `get-namespace-properties` for retrieving properties of a specific namespace.
;; @params:
;; namespace (buff 20): The namespace whose properties are being queried.
(define-read-only (get-namespace-properties (namespace (buff 20)))
    (let (
            ;; Fetch the properties of the specified namespace from the `namespaces` map.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
        )
        ;; Returns the namespace along with its associated properties.
        (ok {
            namespace: namespace,
            properties: namespace-props,
        })
    )
)

;; Read only function to get name properties
(define-read-only (get-bns-info
        (name (buff 48))
        (namespace (buff 20))
    )
    (map-get? name-properties {
        name: name,
        namespace: namespace,
    })
)

;; (new) Defines a read-only function to fetch the unique ID of a BNS name given its name and the namespace it belongs to.
(define-read-only (get-id-from-bns
        (name (buff 48))
        (namespace (buff 20))
    )
    ;; Attempts to retrieve the ID from the 'name-to-index' map using the provided name and namespace as the key.
    (map-get? name-to-index {
        name: name,
        namespace: namespace,
    })
)

;; (new) Defines a read-only function to fetch the BNS name and the namespace given a unique ID.
(define-read-only (get-bns-from-id (id uint))
    ;; Attempts to retrieve the name and namespace from the 'index-to-name' map using the provided id as the key.
    (map-get? index-to-name id)
)

;; (new) Fetcher for primary name
(define-read-only (get-primary-name (owner principal))
    (map-get? primary-name owner)
)

;; (new) Fetcher for primary name returns name and namespace
(define-read-only (get-primary (owner principal))
    (ok (get-bns-from-id (unwrap! (map-get? primary-name owner) ERR-NO-PRIMARY-NAME)))
)

;; public functions
;; @desc (new) SIP-09 compliant function to transfer a token from one owner to another.
;; @param id: ID of the NFT being transferred.
;; @param owner: Principal of the current owner of the NFT.
;; @param recipient: Principal of the recipient of the NFT.
(define-public (transfer
        (id uint)
        (owner principal)
        (recipient principal)
    )
    (let (
            ;; Get the name and namespace of the NFT.
            (name-and-namespace (unwrap! (get-bns-from-id id) ERR-NO-NAME))
            (namespace (get namespace name-and-namespace))
            (name (get name name-and-namespace))
            ;; Get namespace properties and manager.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
            (manager-transfers (get manager-transferable namespace-props))
            ;; Get name properties and owner.
            (name-props (unwrap! (map-get? name-properties name-and-namespace) ERR-NO-NAME))
            (registered-at-value (get registered-at name-props))
            (nft-current-owner (unwrap! (nft-get-owner? BNS-V2 id) ERR-NO-NAME))
        )
        ;; First check if the name was registered
        (match registered-at-value
            is-registered
            ;; If it was registered, check if registered-at is lower than current blockheight
            ;; This check works to make sure that if a name is fast-claimed they have to wait 1 block to transfer it
            (asserts! (< is-registered burn-block-height)
                ERR-OPERATION-UNAUTHORIZED
            )
            ;; If it is not registered then continue
            true
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Check that the namespace is launched
        (asserts! (is-some (get launched-at namespace-props))
            ERR-NAMESPACE-NOT-LAUNCHED
        )
        ;; Check owner and recipient is not the same
        (asserts! (not (is-eq nft-current-owner recipient))
            ERR-OPERATION-UNAUTHORIZED
        )
        ;; We only need to check if manager transfers are true or false, if true then they have to do transfers through the manager contract that calls into mng-transfer, if false then they can call into this function
        (asserts! (not manager-transfers) ERR-NOT-AUTHORIZED)
        ;; Check contract-caller
        (asserts! (is-eq contract-caller nft-current-owner) ERR-NOT-AUTHORIZED)
        ;; Check if in fact the owner is-eq to nft-current-owner
        (asserts! (is-eq owner nft-current-owner) ERR-NOT-AUTHORIZED)
        ;; Ensures the NFT is not currently listed in the market.
        (asserts! (is-none (map-get? market id)) ERR-LISTED)
        ;; Update the name properties with the new owner
        (map-set name-properties name-and-namespace
            (merge name-props { owner: recipient })
        )
        ;; Update primary name if needed for owner
        (update-primary-name-owner id owner)
        ;; Update primary name if needed for recipient
        (update-primary-name-recipient id recipient)
        ;; Execute the NFT transfer.
        (try! (nft-transfer? BNS-V2 id nft-current-owner recipient))
        (print {
            topic: "transfer-name",
            owner: recipient,
            name: {
                name: name,
                namespace: namespace,
            },
            id: id,
            properties: (map-get? name-properties {
                name: name,
                namespace: namespace,
            }),
        })
        (ok true)
    )
)

;; @desc (new) manager function to be called by managed namespaces that allows manager transfers.
;; @param id: ID of the NFT being transferred.
;; @param owner: Principal of the current owner of the NFT.
;; @param recipient: Principal of the recipient of the NFT.
(define-public (mng-transfer
        (id uint)
        (owner principal)
        (recipient principal)
    )
    (let (
            ;; Get the name and namespace of the NFT.
            (name-and-namespace (unwrap! (get-bns-from-id id) ERR-NO-NAME))
            (namespace (get namespace name-and-namespace))
            (name (get name name-and-namespace))
            ;; Get namespace properties and manager.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
            (manager-transfers (get manager-transferable namespace-props))
            (manager (get namespace-manager namespace-props))
            ;; Get name properties and owner.
            (name-props (unwrap! (map-get? name-properties name-and-namespace) ERR-NO-NAME))
            (registered-at-value (get registered-at name-props))
            (nft-current-owner (unwrap! (nft-get-owner? BNS-V2 id) ERR-NO-NAME))
        )
        ;; First check if the name was registered
        (match registered-at-value
            is-registered
            ;; If it was registered, check if registered-at is lower than current blockheight
            ;; This check works to make sure that if a name is fast-claimed they have to wait 1 block to transfer it
            (asserts! (< is-registered burn-block-height)
                ERR-OPERATION-UNAUTHORIZED
            )
            ;; If it is not registered then continue
            true
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Check that the namespace is launched
        (asserts! (is-some (get launched-at namespace-props))
            ERR-NAMESPACE-NOT-LAUNCHED
        )
        ;; Check owner and recipient is not the same
        (asserts! (not (is-eq nft-current-owner recipient))
            ERR-OPERATION-UNAUTHORIZED
        )
        ;; We only need to check if manager transfers are true or false, if true then continue, if false then they can call into `transfer` function
        (asserts! manager-transfers ERR-NOT-AUTHORIZED)
        ;; Check contract-caller, we unwrap-panic because if manager-transfers is true then there has to be a manager
        (asserts! (is-eq contract-caller (unwrap-panic manager))
            ERR-NOT-AUTHORIZED
        )
        ;; Check if in fact the owner is-eq to nft-current-owner
        (asserts! (is-eq owner nft-current-owner) ERR-NOT-AUTHORIZED)
        ;; Ensures the NFT is not currently listed in the market.
        (asserts! (is-none (map-get? market id)) ERR-LISTED)
        ;; Update primary name if needed for owner
        (update-primary-name-owner id owner)
        ;; Update primary name if needed for recipient
        (update-primary-name-recipient id recipient)
        ;; Update the name properties with the new owner
        (map-set name-properties name-and-namespace
            (merge name-props { owner: recipient })
        )
        ;; Execute the NFT transfer.
        (try! (nft-transfer? BNS-V2 id nft-current-owner recipient))
        (print {
            topic: "transfer-name",
            owner: recipient,
            name: {
                name: name,
                namespace: namespace,
            },
            id: id,
            properties: (map-get? name-properties {
                name: name,
                namespace: namespace,
            }),
        })
        (ok true)
    )
)

;; @desc (new) Function to list an NFT for sale.
;; @param id: ID of the NFT being listed.
;; @param price: Listing price.
;; @param comm-trait: Address of the commission-trait.
(define-public (list-in-ustx
        (id uint)
        (price uint)
        (comm-trait <commission-trait>)
    )
    (let (
            ;; Get the name and namespace of the NFT.
            (name-and-namespace (unwrap! (map-get? index-to-name id) ERR-NO-NAME))
            (namespace (get namespace name-and-namespace))
            ;; Get namespace properties and manager.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
            (namespace-manager (get namespace-manager namespace-props))
            ;; Get name properties and registered-at value.
            (name-props (unwrap! (map-get? name-properties name-and-namespace) ERR-NO-NAME))
            (registered-at-value (get registered-at name-props))
            ;; Creates a listing record with price and commission details
            (listing {
                price: price,
                commission: (contract-of comm-trait),
            })
        )
        ;; Checks if the name was registered
        (match registered-at-value
            is-registered
            ;; If it was registered, check if registered-at is lower than current blockheight
            ;; Same as transfers, this check works to make sure that if a name is fast-claimed they have to wait 1 block to list it
            (asserts! (< is-registered burn-block-height)
                ERR-OPERATION-UNAUTHORIZED
            )
            ;; If it is not registered then continue
            true
        )
        ;; Check if there is a namespace manager
        (match namespace-manager
            manager
            ;; If there is then check that the contract-caller is the manager
            (asserts! (is-eq manager contract-caller) ERR-NOT-AUTHORIZED)
            ;; If there isn't assert that the owner is the contract-caller
            (asserts! (is-eq (some contract-caller) (nft-get-owner? BNS-V2 id))
                ERR-NOT-AUTHORIZED
            )
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Updates the market map with the new listing details
        (map-set market id listing)
        ;; Prints listing details
        (ok (print (merge listing {
            a: "list-in-ustx",
            id: id,
        })))
    )
)

;; @desc (new) Function to remove an NFT listing from the market.
;; @param id: ID of the NFT being unlisted.
(define-public (unlist-in-ustx (id uint))
    (let (
            ;; Get the name and namespace of the NFT.
            (name-and-namespace (unwrap! (map-get? index-to-name id) ERR-NO-NAME))
            (namespace (get namespace name-and-namespace))
            ;; Verify if the NFT is listed in the market.
            (market-map (unwrap! (map-get? market id) ERR-NOT-LISTED))
            ;; Get namespace properties and manager.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
            (namespace-manager (get namespace-manager namespace-props))
        )
        ;; Check if there is a namespace manager
        (match namespace-manager
            manager
            ;; If there is then check that the contract-caller is the manager
            (asserts! (is-eq manager contract-caller) ERR-NOT-AUTHORIZED)
            ;; If there isn't assert that the owner is the contract-caller
            (asserts! (is-eq (some contract-caller) (nft-get-owner? BNS-V2 id))
                ERR-NOT-AUTHORIZED
            )
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Deletes the listing from the market map
        (map-delete market id)
        ;; Prints unlisting details
        (ok (print {
            a: "unlist-in-ustx",
            id: id,
        }))
    )
)

;; @desc (new) Function to buy an NFT listed for sale, transferring ownership and handling commission.
;; @param id: ID of the NFT being purchased.
;; @param comm-trait: Address of the commission-trait.
(define-public (buy-in-ustx
        (id uint)
        (comm-trait <commission-trait>)
    )
    (let (
            ;; Retrieves current owner and listing details
            (owner (unwrap! (nft-get-owner? BNS-V2 id) ERR-NO-NAME))
            (listing (unwrap! (map-get? market id) ERR-NOT-LISTED))
            (price (get price listing))
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Verifies the commission details match the listing
        (asserts! (is-eq (contract-of comm-trait) (get commission listing))
            ERR-WRONG-COMMISSION
        )
        ;; Transfers STX from buyer to seller
        (try! (stx-transfer? price contract-caller owner))
        ;; Handle commission payment
        (try! (contract-call? comm-trait pay id price))
        ;; Transfers the NFT to the buyer
        ;; This function differs from the `transfer` method by not checking who the contract-caller is, otherwise trasnfers would never be executed
        (try! (purchase-transfer id owner contract-caller))
        ;; Removes the listing from the market map
        (map-delete market id)
        ;; Prints purchase details
        (ok (print {
            a: "buy-in-ustx",
            id: id,
        }))
    )
)

;; @desc (new) Sets the primary name for the caller to a specific BNS name they own.
;; @param primary-name-id: ID of the name to be set as primary.
(define-public (set-primary-name (primary-name-id uint))
    (begin
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Verify the contract-caller is the owner of the name.
        (asserts!
            (is-eq (unwrap! (nft-get-owner? BNS-V2 primary-name-id) ERR-NO-NAME)
                contract-caller
            )
            ERR-NOT-AUTHORIZED
        )
        ;; Update the contract-caller's primary name.
        (map-set primary-name contract-caller primary-name-id)
        ;; Return true upon successful execution.
        (ok true)
    )
)

;; @desc (new) Defines a public function to burn an NFT, under managed namespaces.
;; @param id: ID of the NFT to be burned.
(define-public (mng-burn (id uint))
    (let (
            ;; Get the name details associated with the given ID.
            (name-and-namespace (unwrap! (get-bns-from-id id) ERR-NO-NAME))
            ;; Get the owner of the name.
            (owner (unwrap! (nft-get-owner? BNS-V2 id) ERR-UNWRAP))
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Ensure the caller is the current namespace manager.
        (asserts!
            (is-eq contract-caller
                (unwrap!
                    (get namespace-manager
                        (unwrap!
                            (map-get? namespaces
                                (get namespace name-and-namespace)
                            )
                            ERR-NAMESPACE-NOT-FOUND
                        ))
                    ERR-NO-NAMESPACE-MANAGER
                ))
            ERR-NOT-AUTHORIZED
        )
        ;; Unlist the NFT if it is listed.
        (match (map-get? market id)
            listed-name (map-delete market id)
            true
        )
        ;; Update primary name if needed for the owner of the name
        (update-primary-name-owner id owner)
        ;; Delete the name from all maps:
        ;; Remove the name-to-index.
        (map-delete name-to-index name-and-namespace)
        ;; Remove the index-to-name.
        (map-delete index-to-name id)
        ;; Remove the name-properties.
        (map-delete name-properties name-and-namespace)
        ;; Executes the burn operation for the specified NFT.
        (try! (nft-burn? BNS-V2 id (unwrap! (nft-get-owner? BNS-V2 id) ERR-UNWRAP)))
        (print {
            topic: "burn-name",
            owner: "",
            name: {
                name: (get name name-and-namespace),
                namespace: (get namespace name-and-namespace),
            },
            id: id,
        })
        (ok true)
    )
)

;; @desc (new) Transfers the management role of a specific namespace to a new principal.
;; @param new-manager: Principal of the new manager.
;; @param namespace: Buffer of the namespace.
(define-public (mng-manager-transfer
        (new-manager (optional principal))
        (namespace (buff 20))
    )
    (let (
            ;; Retrieve namespace properties and current manager.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Ensure the caller is the current namespace manager.
        (asserts!
            (is-eq contract-caller
                (unwrap! (get namespace-manager namespace-props)
                    ERR-NO-NAMESPACE-MANAGER
                ))
            ERR-NOT-AUTHORIZED
        )
        ;; Ensure manager can be changed
        (asserts! (not (get manager-frozen namespace-props)) ERR-NOT-AUTHORIZED)
        ;; Update the namespace manager to the new manager.
        (map-set namespaces namespace
            (merge
                namespace-props
                { namespace-manager: new-manager }
            ))
        (print {
            namespace: namespace,
            status: "transfer-manager",
            properties: (map-get? namespaces namespace),
        })
        (ok true)
    )
)

;; @desc (new) freezes the ability to make manager transfers
;; @param namespace: Buffer of the namespace.
(define-public (freeze-manager (namespace (buff 20)))
    (let (
            ;; Retrieve namespace properties and current manager.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Ensure the caller is the current namespace manager.
        (asserts!
            (is-eq contract-caller
                (unwrap! (get namespace-manager namespace-props)
                    ERR-NO-NAMESPACE-MANAGER
                ))
            ERR-NOT-AUTHORIZED
        )
        ;; Update the namespace manager to the new manager.
        (map-set namespaces namespace
            (merge
                namespace-props
                { manager-frozen: true }
            ))
        (print {
            namespace: namespace,
            status: "freeze-manager",
            properties: (map-get? namespaces namespace),
        })
        (ok true)
    )
)

;;;; NAMESPACES
;; @desc Public function `namespace-preorder` initiates the registration process for a namespace by sending a transaction with a salted hash of the namespace.
;; This transaction burns the registration fee as a commitment.
;; @params: hashed-salted-namespace (buff 20): The hashed and salted namespace being preordered.
;; @params: stx-to-burn (uint): The amount of STX tokens to be burned as part of the preorder process.
(define-public (namespace-preorder
        (hashed-salted-namespace (buff 20))
        (stx-to-burn uint)
    )
    (begin
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Validate that the hashed-salted-namespace is exactly 20 bytes long.
        (asserts! (is-eq (len hashed-salted-namespace) HASH160LEN)
            ERR-HASH-MALFORMED
        )
        ;; Check if the same hashed-salted-fqn has been used before
        (asserts!
            (is-none (map-get? namespace-single-preorder hashed-salted-namespace))
            ERR-PREORDERED-BEFORE
        )
        ;; Confirm that the STX amount to be burned is positive
        (asserts! (> stx-to-burn u0) ERR-STX-BURNT-INSUFFICIENT)
        ;; Execute the token burn operation.
        (try! (stx-burn? stx-to-burn contract-caller))
        ;; Record the preorder details in the `namespace-preorders` map
        (map-set namespace-preorders
            {
                hashed-salted-namespace: hashed-salted-namespace,
                buyer: contract-caller,
            }
            {
                created-at: burn-block-height,
                stx-burned: stx-to-burn,
                claimed: false,
            })
        ;; Sets the map with just the hashed-salted-namespace as the key
        (map-set namespace-single-preorder hashed-salted-namespace true)
        ;; Return the block height at which the preorder claimability expires.
        (ok (+ burn-block-height PREORDER-CLAIMABILITY-TTL))
    )
)

;; @desc Public function `namespace-reveal` completes the second step in the namespace registration process.
;; It associates the revealed namespace with its corresponding preorder, establishes the namespace's pricing function, and sets its lifetime and ownership details.
;; @param: namespace (buff 20): The namespace being revealed.
;; @param: namespace-salt (buff 20): The salt used during the preorder to generate a unique hash.
;; @param: p-func-base, p-func-coeff, p-func-b1 to p-func-b16: Parameters defining the price function for registering names within this namespace.
;; @param: p-func-non-alpha-discount (uint): Discount applied to names with non-alphabetic characters.
;; @param: p-func-no-vowel-discount (uint): Discount applied to names without vowels.
;; @param: lifetime (uint): Duration that names within this namespace are valid before needing renewal.
;; @param: namespace-import (principal): The principal authorized to import names into this namespace.
;; @param: namespace-manager (optional principal): The principal authorized to manage the namespace.
(define-public (namespace-reveal
        (namespace (buff 20))
        (namespace-salt (buff 20))
        (p-func-base uint)
        (p-func-coeff uint)
        (p-func-b1 uint)
        (p-func-b2 uint)
        (p-func-b3 uint)
        (p-func-b4 uint)
        (p-func-b5 uint)
        (p-func-b6 uint)
        (p-func-b7 uint)
        (p-func-b8 uint)
        (p-func-b9 uint)
        (p-func-b10 uint)
        (p-func-b11 uint)
        (p-func-b12 uint)
        (p-func-b13 uint)
        (p-func-b14 uint)
        (p-func-b15 uint)
        (p-func-b16 uint)
        (p-func-non-alpha-discount uint)
        (p-func-no-vowel-discount uint)
        (lifetime uint)
        (namespace-import principal)
        (namespace-manager (optional principal))
        (can-update-price bool)
        (manager-transfers bool)
        (manager-frozen bool)
    )
    (let (
            ;; Generate the hashed, salted namespace identifier to match with its preorder.
            (hashed-salted-namespace (hash160 (concat (concat namespace 0x2e) namespace-salt)))
            ;; Define the price function based on the provided parameters.
            (price-function {
                buckets: (list p-func-b1 p-func-b2 p-func-b3 p-func-b4 p-func-b5 p-func-b6
                    p-func-b7 p-func-b8 p-func-b9 p-func-b10 p-func-b11
                    p-func-b12 p-func-b13 p-func-b14 p-func-b15 p-func-b16),
                base: p-func-base,
                coeff: p-func-coeff,
                nonalpha-discount: p-func-non-alpha-discount,
                no-vowel-discount: p-func-no-vowel-discount,
            })
            ;; Retrieve the preorder record to ensure it exists and is valid for the revealing namespace
            (preorder (unwrap!
                (map-get? namespace-preorders {
                    hashed-salted-namespace: hashed-salted-namespace,
                    buyer: contract-caller,
                })
                ERR-PREORDER-NOT-FOUND
            ))
            ;; Calculate the namespace's registration price for validation.
            (namespace-price (try! (get-namespace-price namespace)))
        )
        ;; Ensure the preorder has not been claimed before
        (asserts! (not (get claimed preorder)) ERR-NAMESPACE-ALREADY-EXISTS)
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Ensure the namespace consists of valid characters only.
        (asserts! (not (has-invalid-chars namespace)) ERR-CHARSET-INVALID)
        ;; Check that the namespace is available for reveal.
        (asserts!
            (unwrap! (can-namespace-be-registered namespace)
                ERR-NAMESPACE-ALREADY-EXISTS
            )
            ERR-NAMESPACE-ALREADY-EXISTS
        )
        ;; Verify the burned amount during preorder meets or exceeds the namespace's registration price.
        (asserts! (>= (get stx-burned preorder) namespace-price)
            ERR-STX-BURNT-INSUFFICIENT
        )
        ;; Confirm the reveal action is performed within the allowed timeframe from the preorder.
        (asserts!
            (< burn-block-height
                (+ (get created-at preorder) PREORDER-CLAIMABILITY-TTL)
            )
            ERR-PREORDER-CLAIMABILITY-EXPIRED
        )
        ;; Ensure at least 1 block has passed after the preorder to avoid namespace sniping.
        (asserts! (>= burn-block-height (+ (get created-at preorder) u1))
            ERR-OPERATION-UNAUTHORIZED
        )
        ;; Check if the namespace manager is assigned
        (match namespace-manager
            namespace-m
            ;; If namespace-manager is assigned, then assign everything except the lifetime, that is set to u0 sinces renewals will be made in the namespace manager contract and set the can update price function to false, since no changes will ever need to be made there.
            (map-set namespaces namespace
                {
                    namespace-manager: namespace-manager,
                    manager-transferable: manager-transfers,
                    manager-frozen: manager-frozen,
                    namespace-import: namespace-import,
                    revealed-at: burn-block-height,
                    launched-at: none,
                    lifetime: u0,
                    can-update-price-function: can-update-price,
                    price-function: price-function,
                })
            ;; If no manager is assigned
            (map-set namespaces namespace
                {
                    namespace-manager: none,
                    manager-transferable: manager-transfers,
                    manager-frozen: manager-frozen,
                    namespace-import: namespace-import,
                    revealed-at: burn-block-height,
                    launched-at: none,
                    lifetime: lifetime,
                    can-update-price-function: can-update-price,
                    price-function: price-function,
                })
        )
        ;; Update the claimed value for the preorder
        (map-set namespace-preorders {
            hashed-salted-namespace: hashed-salted-namespace,
            buyer: contract-caller,
        }
            (merge preorder
                { claimed: true }
            ))
        ;; Confirm successful reveal of the namespace
        (ok true)
    )
)

;; @desc Public function `namespace-launch` marks a namespace as launched and available for public name registrations.
;; @param: namespace (buff 20): The namespace to be launched and made available for public registrations.
(define-public (namespace-launch (namespace (buff 20)))
    (let (
            ;; Retrieve the properties of the namespace to ensure it exists and to check its current state.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Ensure the transaction sender is the namespace's designated import principal.
        (asserts! (is-eq (get namespace-import namespace-props) contract-caller)
            ERR-OPERATION-UNAUTHORIZED
        )
        ;; Verify the namespace has not already been launched.
        (asserts! (is-none (get launched-at namespace-props))
            ERR-NAMESPACE-ALREADY-LAUNCHED
        )
        ;; Confirm that the action is taken within the permissible time frame since the namespace was revealed.
        (asserts!
            (< burn-block-height
                (+ (get revealed-at namespace-props) NAMESPACE-LAUNCHABILITY-TTL)
            )
            ERR-NAMESPACE-PREORDER-LAUNCHABILITY-EXPIRED
        )
        ;; Update the `namespaces` map with the newly launched status.
        (map-set namespaces namespace
            (merge namespace-props { launched-at: (some burn-block-height) })
        )
        ;; Emit an event to indicate the namespace is now ready and launched.
        (print {
            namespace: namespace,
            status: "launch",
            properties: (map-get? namespaces namespace),
        })
        ;; Confirm the successful launch of the namespace.
        (ok true)
    )
)

;; @desc (new) Public function `turn-off-manager-transfers` disables manager transfers for a namespace (callable only once).
;; @param: namespace (buff 20): The namespace for which manager transfers will be disabled.
(define-public (turn-off-manager-transfers (namespace (buff 20)))
    (let (
            ;; Retrieve the properties of the namespace and manager.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
            (namespace-manager (unwrap! (get namespace-manager namespace-props)
                ERR-NO-NAMESPACE-MANAGER
            ))
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Ensure the function caller is the namespace manager.
        (asserts! (is-eq contract-caller namespace-manager) ERR-NOT-AUTHORIZED)
        ;; Disable manager transfers.
        (map-set namespaces namespace
            (merge namespace-props { manager-transferable: false })
        )
        (print {
            namespace: namespace,
            status: "turn-off-manager-transfers",
            properties: (map-get? namespaces namespace),
        })
        ;; Confirm successful execution.
        (ok true)
    )
)

;; @desc Public function `name-import` allows the insertion of names into a namespace that has been revealed but not yet launched.
;; This facilitates pre-populating the namespace with specific names, assigning owners.
;; @param: namespace (buff 20): The namespace into which the name is being imported.
;; @param: name (buff 48): The name being imported into the namespace.
;; @param: beneficiary (principal): The principal who will own the imported name.
;; @param: stx-burn (uint): The amount of STX tokens to be burned as part of the import process.
(define-public (name-import
        (namespace (buff 20))
        (name (buff 48))
        (beneficiary principal)
    )
    (let (
            ;; Fetch properties of the specified namespace.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
            ;; Fetch the latest index to mint
            (current-mint (+ (var-get bns-index) u1))
            (price (if (is-none (get namespace-manager namespace-props))
                (try! (compute-name-price name (get price-function namespace-props)))
                u0
            ))
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Ensure the name is not already registered.
        (asserts!
            (is-none (map-get? name-properties {
                name: name,
                namespace: namespace,
            }))
            ERR-NAME-NOT-AVAILABLE
        )
        ;; Verify that the name contains only valid characters.
        (asserts! (not (has-invalid-chars name)) ERR-CHARSET-INVALID)
        ;; Ensure the contract-caller is the namespace's designated import principal or the namespace manager
        (asserts!
            (or (is-eq (get namespace-import namespace-props) contract-caller) (is-eq (get namespace-manager namespace-props) (some contract-caller)))
            ERR-OPERATION-UNAUTHORIZED
        )
        ;; Check that the namespace has not been launched yet, as names can only be imported to namespaces that are revealed but not launched.
        (asserts! (is-none (get launched-at namespace-props))
            ERR-NAMESPACE-ALREADY-LAUNCHED
        )
        ;; Confirm that the import is occurring within the allowed timeframe since the namespace was revealed.
        (asserts!
            (< burn-block-height
                (+ (get revealed-at namespace-props) NAMESPACE-LAUNCHABILITY-TTL)
            )
            ERR-NAMESPACE-PREORDER-LAUNCHABILITY-EXPIRED
        )
        ;; Set the name properties
        (map-set name-properties {
            name: name,
            namespace: namespace,
        }
            {
                registered-at: none,
                imported-at: (some burn-block-height),
                hashed-salted-fqn-preorder: none,
                preordered-by: none,
                renewal-height: u0,
                stx-burn: price,
                owner: beneficiary,
            })
        (map-set name-to-index {
            name: name,
            namespace: namespace,
        }
            current-mint
        )
        (map-set index-to-name current-mint {
            name: name,
            namespace: namespace,
        })
        ;; Update primary name if needed for send-to
        (update-primary-name-recipient current-mint beneficiary)
        ;; Update the index of the minting
        (var-set bns-index current-mint)
        ;; Mint the name to the beneficiary
        (try! (nft-mint? BNS-V2 current-mint beneficiary))
        ;; Log the new name registration
        (print {
            topic: "new-name",
            owner: beneficiary,
            name: {
                name: name,
                namespace: namespace,
            },
            id: current-mint,
            properties: (map-get? name-properties {
                name: name,
                namespace: namespace,
            }),
        })
        ;; Confirm successful import of the name.
        (ok true)
    )
)

;; @desc Public function `namespace-update-price` updates the pricing function for a specific namespace.
;; @param: namespace (buff 20): The namespace for which the price function is being updated.
;; @param: p-func-base (uint): The base price used in the pricing function.
;; @param: p-func-coeff (uint): The coefficient used in the pricing function.
;; @param: p-func-b1 to p-func-b16 (uint): The bucket-specific multipliers for the pricing function.
;; @param: p-func-non-alpha-discount (uint): The discount applied for non-alphabetic characters.
;; @param: p-func-no-vowel-discount (uint): The discount applied when no vowels are present.
(define-public (namespace-update-price
        (namespace (buff 20))
        (p-func-base uint)
        (p-func-coeff uint)
        (p-func-b1 uint)
        (p-func-b2 uint)
        (p-func-b3 uint)
        (p-func-b4 uint)
        (p-func-b5 uint)
        (p-func-b6 uint)
        (p-func-b7 uint)
        (p-func-b8 uint)
        (p-func-b9 uint)
        (p-func-b10 uint)
        (p-func-b11 uint)
        (p-func-b12 uint)
        (p-func-b13 uint)
        (p-func-b14 uint)
        (p-func-b15 uint)
        (p-func-b16 uint)
        (p-func-non-alpha-discount uint)
        (p-func-no-vowel-discount uint)
    )
    (let (
            ;; Retrieve the current properties of the namespace.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
            ;; Construct the new price function.
            (price-function {
                buckets: (list p-func-b1 p-func-b2 p-func-b3 p-func-b4 p-func-b5 p-func-b6
                    p-func-b7 p-func-b8 p-func-b9 p-func-b10 p-func-b11
                    p-func-b12 p-func-b13 p-func-b14 p-func-b15 p-func-b16),
                base: p-func-base,
                coeff: p-func-coeff,
                nonalpha-discount: p-func-non-alpha-discount,
                no-vowel-discount: p-func-no-vowel-discount,
            })
        )
        (match (get namespace-manager namespace-props)
            manager
            ;; Ensure that the transaction sender is the namespace's designated import principal.
            (asserts! (is-eq manager contract-caller) ERR-OPERATION-UNAUTHORIZED)
            ;; Ensure that the contract-caller is the namespace's designated import principal.
            (asserts!
                (is-eq (get namespace-import namespace-props) contract-caller)
                ERR-OPERATION-UNAUTHORIZED
            )
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Verify the namespace's price function can still be updated.
        (asserts! (get can-update-price-function namespace-props)
            ERR-OPERATION-UNAUTHORIZED
        )
        ;; Update the namespace's record in the `namespaces` map with the new price function.
        (map-set namespaces namespace
            (merge namespace-props { price-function: price-function })
        )
        (print {
            namespace: namespace,
            status: "update-price-manager",
            properties: (map-get? namespaces namespace),
        })
        ;; Confirm the successful update of the price function.
        (ok true)
    )
)

;; @desc Public function `namespace-freeze-price` disables the ability to update the price function for a given namespace.
;; @param: namespace (buff 20): The target namespace for which the price function update capability is being revoked.
(define-public (namespace-freeze-price (namespace (buff 20)))
    (let (
            ;; Retrieve the properties of the specified namespace to verify its existence and fetch its current settings.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
        )
        (match (get namespace-manager namespace-props)
            manager
            ;; Ensure that the transaction sender is the same as the namespace's designated import principal.
            (asserts! (is-eq manager contract-caller) ERR-OPERATION-UNAUTHORIZED)
            ;; Ensure that the contract-caller is the same as the namespace's designated import principal.
            (asserts!
                (is-eq (get namespace-import namespace-props) contract-caller)
                ERR-OPERATION-UNAUTHORIZED
            )
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Update the namespace properties in the `namespaces` map, setting `can-update-price-function` to false.
        (map-set namespaces namespace
            (merge namespace-props { can-update-price-function: false })
        )
        (print {
            namespace: namespace,
            status: "freeze-price-manager",
            properties: (map-get? namespaces namespace),
        })
        ;; Return a success confirmation.
        (ok true)
    )
)

;; @desc (new) A 'fast' one-block registration function: (name-claim-fast)
;; Warning: this *is* snipeable, for a slower but un-snipeable claim, use the pre-order & register functions
;; @param: name (buff 48): The name being claimed.
;; @param: namespace (buff 20): The namespace under which the name is being claimed.
;; @param: stx-burn (uint): The amount of STX to burn for the claim.
;; @param: send-to (principal): The principal to whom the name will be sent.
(define-public (name-claim-fast
        (name (buff 48))
        (namespace (buff 20))
        (send-to principal)
    )
    (let (
            ;; Retrieve namespace properties.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
            (current-namespace-manager (get namespace-manager namespace-props))
            ;; Calculates the ID for the new name to be minted.
            (id-to-be-minted (+ (var-get bns-index) u1))
            ;; Check if the name already exists.
            (name-props (map-get? name-properties {
                name: name,
                namespace: namespace,
            }))
            ;; new to get the price of the name
            (name-price (if (is-none current-namespace-manager)
                (try! (compute-name-price name (get price-function namespace-props)))
                u0
            ))
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Ensure the name is not already registered.
        (asserts! (is-none name-props) ERR-NAME-NOT-AVAILABLE)
        ;; Verify that the name contains only valid characters.
        (asserts! (not (has-invalid-chars name)) ERR-CHARSET-INVALID)
        ;; Ensure that the namespace is launched
        (asserts! (is-some (get launched-at namespace-props))
            ERR-NAMESPACE-NOT-LAUNCHED
        )
        ;; Check namespace manager
        (match current-namespace-manager
            manager
            ;; If manager, check contract-caller is manager
            (asserts! (is-eq contract-caller manager) ERR-NOT-AUTHORIZED)
            ;; If no manager
            (begin
                ;; Asserts contract-caller is the send-to if not a managed namespace
                (asserts! (is-eq contract-caller send-to) ERR-NOT-AUTHORIZED)
                ;; Updated this to burn the actual amount of the name-price
                (try! (stx-burn? name-price send-to))
            )
        )
        ;; Update the index
        (var-set bns-index id-to-be-minted)
        ;; Sets properties for the newly registered name.
        (map-set name-properties
            {
                name: name,
                namespace: namespace,
            }
            {
                registered-at: (some (+ burn-block-height u1)),
                imported-at: none,
                hashed-salted-fqn-preorder: none,
                preordered-by: none,
                ;; Updated this to actually start with the registered-at date/block, and also to be u0 if it is a managed namespace
                renewal-height: (if (is-eq (get lifetime namespace-props) u0)
                    u0
                    (+ (get lifetime namespace-props) burn-block-height u1)
                ),
                stx-burn: name-price,
                owner: send-to,
            })
        (map-set name-to-index {
            name: name,
            namespace: namespace,
        }
            id-to-be-minted
        )
        (map-set index-to-name id-to-be-minted {
            name: name,
            namespace: namespace,
        })
        ;; Update primary name if needed for send-to
        (update-primary-name-recipient id-to-be-minted send-to)
        ;; Mints the new BNS name.
        (try! (nft-mint? BNS-V2 id-to-be-minted send-to))
        ;; Log the new name registration
        (print {
            topic: "new-name",
            owner: send-to,
            name: {
                name: name,
                namespace: namespace,
            },
            id: id-to-be-minted,
            properties: (map-get? name-properties {
                name: name,
                namespace: namespace,
            }),
        })
        ;; Signals successful completion.
        (ok id-to-be-minted)
    )
)

;; @desc Defines a public function `name-preorder` for preordering BNS names by burning the registration fee and submitting the salted hash.
;; Callable by anyone; the actual check for authorization happens in the `name-register` function.
;; @param: hashed-salted-fqn (buff 20): The hashed and salted fully qualified name.
;; @param: stx-to-burn (uint): The amount of STX to burn for the preorder.
(define-public (name-preorder
        (hashed-salted-fqn (buff 20))
        (stx-to-burn uint)
    )
    (begin
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Validate the length of the hashed-salted FQN.
        (asserts! (is-eq (len hashed-salted-fqn) HASH160LEN) ERR-HASH-MALFORMED)
        ;; Ensures that the amount of STX specified to burn is greater than zero.
        (asserts! (> stx-to-burn u0) ERR-STX-BURNT-INSUFFICIENT)
        ;; Check if the same hashed-salted-fqn has been used before
        (asserts! (is-none (map-get? name-single-preorder hashed-salted-fqn))
            ERR-PREORDERED-BEFORE
        )
        ;; Transfers the specified amount of stx to the BNS contract to burn on register
        (try! (stx-transfer? stx-to-burn contract-caller .BNS-V2))
        ;; Records the preorder in the 'name-preorders' map.
        (map-set name-preorders
            {
                hashed-salted-fqn: hashed-salted-fqn,
                buyer: contract-caller,
            }
            {
                created-at: burn-block-height,
                stx-burned: stx-to-burn,
                claimed: false,
            })
        ;; Sets the map with just the hashed-salted-fqn as the key
        (map-set name-single-preorder hashed-salted-fqn true)
        ;; Returns the block height at which the preorder's claimability period will expire.
        (ok (+ burn-block-height PREORDER-CLAIMABILITY-TTL))
    )
)

;; @desc Public function `name-register` finalizes the registration of a BNS name for users from unmanaged namespaces.
;; @param: namespace (buff 20): The namespace to which the name belongs.
;; @param: name (buff 48): The name to be registered.
;; @param: salt (buff 20): The salt used during the preorder.
(define-public (name-register
        (namespace (buff 20))
        (name (buff 48))
        (salt (buff 20))
    )
    (let (
            ;; Generate a unique identifier for the name by hashing the fully-qualified name with salt
            (hashed-salted-fqn (hash160 (concat (concat (concat name 0x2e) namespace) salt)))
            ;; Retrieve the preorder details for this name
            (preorder (unwrap!
                (map-get? name-preorders {
                    hashed-salted-fqn: hashed-salted-fqn,
                    buyer: contract-caller,
                })
                ERR-PREORDER-NOT-FOUND
            ))
            ;; Fetch the properties of the namespace
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
            ;; Get the amount of burned STX
            (stx-burned (get stx-burned preorder))
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Ensure that the namespace is launched
        (asserts! (is-some (get launched-at namespace-props))
            ERR-NAMESPACE-NOT-LAUNCHED
        )
        ;; Ensure the preorder hasn't been claimed before
        (asserts! (not (get claimed preorder)) ERR-OPERATION-UNAUTHORIZED)
        ;; Check that the namespace doesn't have a manager (implying it's open for registration)
        (asserts! (is-none (get namespace-manager namespace-props))
            ERR-NOT-AUTHORIZED
        )
        ;; Verify that the preorder was made after the namespace was launched
        (asserts!
            (> (get created-at preorder)
                (unwrap! (get launched-at namespace-props) ERR-UNWRAP)
            )
            ERR-NAME-PREORDERED-BEFORE-NAMESPACE-LAUNCH
        )
        ;; Ensure the registration is happening within the allowed time window after preorder
        (asserts!
            (< burn-block-height
                (+ (get created-at preorder) PREORDER-CLAIMABILITY-TTL)
            )
            ERR-PREORDER-CLAIMABILITY-EXPIRED
        )
        ;; Make sure at least one block has passed since the preorder (prevents front-running)
        (asserts! (> burn-block-height (+ (get created-at preorder) u1))
            ERR-NAME-NOT-CLAIMABLE-YET
        )
        ;; Verify that enough STX was burned during preorder to cover the name price
        (asserts!
            (is-eq stx-burned
                (try! (compute-name-price name (get price-function namespace-props)))
            )
            ERR-STX-BURNT-INSUFFICIENT
        )
        ;; Verify that the name contains only valid characters.
        (asserts! (not (has-invalid-chars name)) ERR-CHARSET-INVALID)
        ;; Mark the preorder as claimed to prevent double-spending
        (map-set name-preorders {
            hashed-salted-fqn: hashed-salted-fqn,
            buyer: contract-caller,
        }
            (merge preorder { claimed: true })
        )
        ;; Check if the name already exists
        (match (map-get? name-properties {
            name: name,
            namespace: namespace,
        })
            name-props-exist
            ;; If the name exists
            (handle-existing-name name-props-exist hashed-salted-fqn
                (get created-at preorder) stx-burned name namespace
                (get lifetime namespace-props)
            )
            ;; If the name does not exist
            (register-new-name (+ (var-get bns-index) u1) hashed-salted-fqn
                stx-burned name namespace (get lifetime namespace-props)
            )
        )
    )
)

;; @desc (new) Defines a public function `claim-preorder` for claiming back the STX commited to be burnt on registration.
;; This should only be allowed to go through if preorder-claimability-ttl has passed
;; @param: hashed-salted-fqn (buff 20): The hashed and salted fully qualified name.
(define-public (claim-preorder (hashed-salted-fqn (buff 20)))
    (let (
            ;; Retrieves the preorder details.
            (preorder (unwrap!
                (map-get? name-preorders {
                    hashed-salted-fqn: hashed-salted-fqn,
                    buyer: contract-caller,
                })
                ERR-PREORDER-NOT-FOUND
            ))
            (claimer contract-caller)
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Check if the preorder-claimability-ttl has passed
        (asserts!
            (> burn-block-height
                (+ (get created-at preorder) PREORDER-CLAIMABILITY-TTL)
            )
            ERR-OPERATION-UNAUTHORIZED
        )
        ;; Asserts that the preorder has not been claimed
        (asserts! (not (get claimed preorder)) ERR-OPERATION-UNAUTHORIZED)
        ;; Transfers back the specified amount of stx from the BNS contract to the contract-caller
        (try! (as-contract (stx-transfer? (get stx-burned preorder) .BNS-V2 claimer)))
        ;; Deletes the preorder in the 'name-preorders' map.
        (map-delete name-preorders {
            hashed-salted-fqn: hashed-salted-fqn,
            buyer: contract-caller,
        })
        ;; Remove the entry from the name-single-preorder map
        (map-delete name-single-preorder hashed-salted-fqn)
        ;; Returns ok true
        (ok true)
    )
)

;; @desc (new) This function is similar to `name-preorder` but only for namespace managers, without the burning of STX tokens.
;; Intended only for managers as mng-name-register & name-register will validate.
;; @param: hashed-salted-fqn (buff 20): The hashed and salted fully-qualified name (FQN) being preordered.
(define-public (mng-name-preorder (hashed-salted-fqn (buff 20)))
    (begin
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Validates that the length of the hashed and salted FQN is exactly 20 bytes.
        (asserts! (is-eq (len hashed-salted-fqn) HASH160LEN) ERR-HASH-MALFORMED)
        ;; Check if the same hashed-salted-fqn has been used before
        (asserts! (is-none (map-get? name-single-preorder hashed-salted-fqn))
            ERR-PREORDERED-BEFORE
        )
        ;; Records the preorder in the 'name-preorders' map. Buyer set to contract-caller
        (map-set name-preorders
            {
                hashed-salted-fqn: hashed-salted-fqn,
                buyer: contract-caller,
            }
            {
                created-at: burn-block-height,
                stx-burned: u0,
                claimed: false,
            })
        ;; Sets the map with just the hashed-salted-fqn as the key
        (map-set name-single-preorder hashed-salted-fqn true)
        ;; Returns the block height at which the preorder's claimability period will expire.
        (ok (+ burn-block-height PREORDER-CLAIMABILITY-TTL))
    )
)

;; @desc (new) This function uses provided details to verify the preorder, register the name, and assign it initial properties.
;; This should only allow Managers from MANAGED namespaces to register names.
;; @param: namespace (buff 20): The namespace for the name.
;; @param: name (buff 48): The name being registered.
;; @param: salt (buff 20): The salt used in hashing.
;; @param: send-to (principal): The principal to whom the name will be registered.
(define-public (mng-name-register
        (namespace (buff 20))
        (name (buff 48))
        (salt (buff 20))
        (send-to principal)
    )
    (let (
            ;; Generates the hashed, salted fully-qualified name.
            (hashed-salted-fqn (hash160 (concat (concat (concat name 0x2e) namespace) salt)))
            ;; Retrieves the existing properties of the namespace to confirm its existence and management details.
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
            (current-namespace-manager (unwrap! (get namespace-manager namespace-props)
                ERR-NO-NAMESPACE-MANAGER
            ))
            ;; Retrieves the preorder information using the hashed-salted FQN to verify the preorder exists
            (preorder (unwrap!
                (map-get? name-preorders {
                    hashed-salted-fqn: hashed-salted-fqn,
                    buyer: current-namespace-manager,
                })
                ERR-PREORDER-NOT-FOUND
            ))
            ;; Calculates the ID for the new name to be minted.
            (id-to-be-minted (+ (var-get bns-index) u1))
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Ensure the preorder has not been claimed before
        (asserts! (not (get claimed preorder)) ERR-OPERATION-UNAUTHORIZED)
        ;; Ensure the name is not already registered
        (asserts!
            (is-none (map-get? name-properties {
                name: name,
                namespace: namespace,
            }))
            ERR-NAME-NOT-AVAILABLE
        )
        ;; Verify that the name contains only valid characters.
        (asserts! (not (has-invalid-chars name)) ERR-CHARSET-INVALID)
        ;; Verifies that the caller is the namespace manager.
        (asserts! (is-eq contract-caller current-namespace-manager)
            ERR-NOT-AUTHORIZED
        )
        ;; Validates that the preorder was made after the namespace was officially launched.
        (asserts!
            (> (get created-at preorder)
                (unwrap! (get launched-at namespace-props) ERR-UNWRAP)
            )
            ERR-NAME-PREORDERED-BEFORE-NAMESPACE-LAUNCH
        )
        ;; Verifies the registration is completed within the claimability period.
        (asserts!
            (< burn-block-height
                (+ (get created-at preorder) PREORDER-CLAIMABILITY-TTL)
            )
            ERR-PREORDER-CLAIMABILITY-EXPIRED
        )
        ;; Sets properties for the newly registered name.
        (map-set name-properties
            {
                name: name,
                namespace: namespace,
            }
            {
                registered-at: (some burn-block-height),
                imported-at: none,
                hashed-salted-fqn-preorder: (some hashed-salted-fqn),
                preordered-by: (some send-to),
                ;; Updated this to be u0, so that renewals are handled through the namespace manager
                renewal-height: u0,
                stx-burn: u0,
                owner: send-to,
            })
        (map-set name-to-index {
            name: name,
            namespace: namespace,
        }
            id-to-be-minted
        )
        (map-set index-to-name id-to-be-minted {
            name: name,
            namespace: namespace,
        })
        ;; Update primary name if needed for send-to
        (update-primary-name-recipient id-to-be-minted send-to)
        ;; Updates BNS-index variable to the newly minted ID.
        (var-set bns-index id-to-be-minted)
        ;; Update map to claimed for preorder, to avoid people reclaiming stx from an already registered name
        (map-set name-preorders {
            hashed-salted-fqn: hashed-salted-fqn,
            buyer: current-namespace-manager,
        }
            (merge preorder { claimed: true })
        )
        ;; Mints the BNS name as an NFT to the send-to address, finalizing the registration.
        (try! (nft-mint? BNS-V2 id-to-be-minted send-to))
        ;; Log the new name registration
        (print {
            topic: "new-name",
            owner: send-to,
            name: {
                name: name,
                namespace: namespace,
            },
            id: id-to-be-minted,
            properties: (map-get? name-properties {
                name: name,
                namespace: namespace,
            }),
        })
        ;; Confirms successful registration of the name.
        (ok id-to-be-minted)
    )
)

;; Public function `name-renewal` for renewing ownership of a name.
;; @param: namespace (buff 20): The namespace of the name to be renewed.
;; @param: name (buff 48): The actual name to be renewed.
;; @param: stx-to-burn (uint): The amount of STX tokens to be burned for renewal.
(define-public (name-renewal
        (namespace (buff 20))
        (name (buff 48))
    )
    (let (
            ;; Get the unique identifier for this name
            (name-index (unwrap! (get-id-from-bns name namespace) ERR-NO-NAME))
            ;; Retrieve the properties of the namespace
            (namespace-props (unwrap! (map-get? namespaces namespace) ERR-NAMESPACE-NOT-FOUND))
            ;; Get the manager of the namespace, if any
            (namespace-manager (get namespace-manager namespace-props))
            ;; Get the current owner of the name
            (owner (unwrap! (nft-get-owner? BNS-V2 name-index) ERR-NO-NAME))
            ;; Retrieve the properties of the name
            (name-props (unwrap!
                (map-get? name-properties {
                    name: name,
                    namespace: namespace,
                })
                ERR-NO-NAME
            ))
            ;; Get the lifetime of names in this namespace
            (lifetime (get lifetime namespace-props))
            ;; Get the current renewal height of the name
            (renewal-height (try! (get-renewal-height name-index)))
            ;; Calculate the new renewal height based on current block height
            (new-renewal-height (+ burn-block-height lifetime))
        )
        ;; Check if migration is complete
        (asserts! (var-get migration-complete) ERR-MIGRATION-IN-PROGRESS)
        ;; Verify that the namespace has been launched
        (asserts! (is-some (get launched-at namespace-props))
            ERR-NAMESPACE-NOT-LAUNCHED
        )
        ;; Ensure the namespace doesn't have a manager
        (asserts! (is-none namespace-manager) ERR-NAMESPACE-HAS-MANAGER)
        ;; Check if renewals are required for this namespace
        (asserts! (> lifetime u0) ERR-LIFETIME-EQUAL-0)
        ;; Handle renewal based on whether it's within the grace period or not
        (if (< burn-block-height (+ renewal-height NAME-GRACE-PERIOD-DURATION))
            (try! (handle-renewal-in-grace-period name namespace name-props owner
                lifetime new-renewal-height
            ))
            (try! (handle-renewal-after-grace-period name namespace name-props owner
                name-index new-renewal-height
            ))
        )
        ;; Burn the specified amount of STX
        (try! (stx-burn?
            (try! (compute-name-price name (get price-function namespace-props)))
            contract-caller
        ))
        ;; update the new stx-burn to the one paid in renewal
        (map-set name-properties {
            name: name,
            namespace: namespace,
        }
            (merge
                (unwrap-panic (map-get? name-properties {
                    name: name,
                    namespace: namespace,
                })) { stx-burn: (try! (compute-name-price name (get price-function namespace-props))) }
            ))
        ;; Return success
        (ok true)
    )
)

;; Private function to handle renewals within the grace period
(define-private (handle-renewal-in-grace-period
        (name (buff 48))
        (namespace (buff 20))
        (name-props {
            registered-at: (optional uint),
            imported-at: (optional uint),
            hashed-salted-fqn-preorder: (optional (buff 20)),
            preordered-by: (optional principal),
            renewal-height: uint,
            stx-burn: uint,
            owner: principal,
        })
        (owner principal)
        (lifetime uint)
        (new-renewal-height uint)
    )
    (begin
        ;; Ensure only the owner can renew within the grace period
        (asserts! (is-eq contract-caller owner) ERR-NOT-AUTHORIZED)
        ;; Update the name properties with the new renewal height
        (map-set name-properties {
            name: name,
            namespace: namespace,
        }
            (merge name-props
                {
                    renewal-height:
                        ;; If still within lifetime, extend from current renewal height; otherwise, use new renewal height
                        (if (< burn-block-height
                                (unwrap-panic (get-renewal-height (unwrap-panic (get-id-from-bns name namespace))))
                            )
                            (+
                                (unwrap-panic (get-renewal-height (unwrap-panic (get-id-from-bns name namespace))))
                                lifetime
                            )
                            new-renewal-height
                        ),
                })
        )
        (print {
            topic: "renew-name",
            owner: owner,
            name: {
                name: name,
                namespace: namespace,
            },
            id: (get-id-from-bns name namespace),
            properties: (map-get? name-properties {
                name: name,
                namespace: namespace,
            }),
        })
        (ok true)
    )
)

;; Private function to handle renewals after the grace period
(define-private (handle-renewal-after-grace-period
        (name (buff 48))
        (namespace (buff 20))
        (name-props {
            registered-at: (optional uint),
            imported-at: (optional uint),
            hashed-salted-fqn-preorder: (optional (buff 20)),
            preordered-by: (optional principal),
            renewal-height: uint,
            stx-burn: uint,
            owner: principal,
        })
        (owner principal)
        (name-index uint)
        (new-renewal-height uint)
    )
    (if (is-eq contract-caller owner)
        ;; If the owner is renewing, simply update the renewal height
        (ok (map-set name-properties {
            name: name,
            namespace: namespace,
        }
            (merge name-props { renewal-height: new-renewal-height })
        ))
        ;; If someone else is renewing (taking over the name)
        (begin
            ;; Check if the name is listed on the market and remove the listing if it is
            (match (map-get? market name-index)
                listed-name (map-delete market name-index)
                true
            )
            (map-set name-properties {
                name: name,
                namespace: namespace,
            }
                (merge name-props { renewal-height: new-renewal-height })
            )
            ;; Update the name properties with the new renewal height and owner
            (ok (try! (purchase-transfer name-index owner contract-caller)))
        )
    )
)

;; Returns the minimum of two uint values.
(define-private (min
        (a uint)
        (b uint)
    )
    ;; If 'a' is less than or equal to 'b', return 'a', else return 'b'.
    (if (<= a b)
        a
        b
    )
)

;; Returns the maximum of two uint values.
(define-private (max
        (a uint)
        (b uint)
    )
    ;; If 'a' is greater than 'b', return 'a', else return 'b'.
    (if (> a b)
        a
        b
    )
)

;; Retrieves an exponent value from a list of buckets based on the provided index.
(define-private (get-exp-at-index
        (buckets (list 16 uint))
        (index uint)
    )
    ;; Retrieves the element at the specified index.
    (unwrap-panic (element-at? buckets index))
)

;; Determines if a character is a digit (0-9).
(define-private (is-digit (char (buff 1)))
    (or
        ;; Checks if the character is between '0' and '9' using hex values.
        (is-eq char 0x30) ;; 0
        (is-eq char 0x31) ;; 1
        (is-eq char 0x32) ;; 2
        (is-eq char 0x33) ;; 3
        (is-eq char 0x34) ;; 4
        (is-eq char 0x35) ;; 5
        (is-eq char 0x36) ;; 6
        (is-eq char 0x37) ;; 7
        (is-eq char 0x38) ;; 8
        (is-eq char 0x39) ;; 9
    )
)

;; Checks if a character is a lowercase alphabetic character (a-z).
(define-private (is-lowercase-alpha (char (buff 1)))
    (or
        ;; Checks for each lowercase letter using hex values.
        (is-eq char 0x61) ;; a
        (is-eq char 0x62) ;; b
        (is-eq char 0x63) ;; c
        (is-eq char 0x64) ;; d
        (is-eq char 0x65) ;; e
        (is-eq char 0x66) ;; f
        (is-eq char 0x67) ;; g
        (is-eq char 0x68) ;; h
        (is-eq char 0x69) ;; i
        (is-eq char 0x6a) ;; j
        (is-eq char 0x6b) ;; k
        (is-eq char 0x6c) ;; l
        (is-eq char 0x6d) ;; m
        (is-eq char 0x6e) ;; n
        (is-eq char 0x6f) ;; o
        (is-eq char 0x70) ;; p
        (is-eq char 0x71) ;; q
        (is-eq char 0x72) ;; r
        (is-eq char 0x73) ;; s
        (is-eq char 0x74) ;; t
        (is-eq char 0x75) ;; u
        (is-eq char 0x76) ;; v
        (is-eq char 0x77) ;; w
        (is-eq char 0x78) ;; x
        (is-eq char 0x79) ;; y
        (is-eq char 0x7a) ;; z
    )
)

;; Determines if a character is a vowel (a, e, i, o, u, and y).
(define-private (is-vowel (char (buff 1)))
    (or
        (is-eq char 0x61) ;; a
        (is-eq char 0x65) ;; e
        (is-eq char 0x69) ;; i
        (is-eq char 0x6f) ;; o
        (is-eq char 0x75) ;; u
        (is-eq char 0x79) ;; y
    )
)

;; Identifies if a character is a special character, specifically '-' or '_'.
(define-private (is-special-char (char (buff 1)))
    (or
        (is-eq char 0x2d) ;; -
        (is-eq char 0x5f)
    )
    ;; _
)

;; Determines if a character is valid within a name, based on allowed character sets.
(define-private (is-char-valid (char (buff 1)))
    (or
        (is-lowercase-alpha char)
        (is-digit char)
        (is-special-char char)
    )
)

;; Checks if a character is non-alphabetic, either a digit or a special character.
(define-private (is-nonalpha (char (buff 1)))
    (or (is-digit char) (is-special-char char))
)

;; Evaluates if a name contains any vowel characters.
(define-private (has-vowels-chars (name (buff 48)))
    (> (len (filter is-vowel name)) u0)
)

;; Determines if a name contains non-alphabetic characters.
(define-private (has-nonalpha-chars (name (buff 48)))
    (> (len (filter is-nonalpha name)) u0)
)

;; Identifies if a name contains any characters that are not considered valid.
(define-private (has-invalid-chars (name (buff 48)))
    (< (len (filter is-char-valid name)) (len name))
)

;; Private helper function `is-namespace-available` checks if a namespace is available for registration or other operations.
;; It considers if the namespace has been launched and whether it has expired.
;; @params:
;; namespace (buff 20): The namespace to check for availability.
(define-private (is-namespace-available (namespace (buff 20)))
    ;; Check if the namespace exists
    (match (map-get? namespaces namespace)
        namespace-props
        ;; If it exists
        ;; Check if the namespace has been launched.
        (match (get launched-at namespace-props)
            launched
            ;; If the namespace is launched, it's considered unavailable if it hasn't expired.
            false
            ;; Check if the namespace is expired by comparing the current block height to the reveal time plus the launchability TTL.
            (> burn-block-height
                (+ (get revealed-at namespace-props) NAMESPACE-LAUNCHABILITY-TTL)
            )
        )
        ;; If the namespace doesn't exist in the map, it's considered available.
        true
    )
)

;; Private helper function `compute-name-price` calculates the registration price for a name based on its length and character composition.
;; It utilizes a configurable pricing function that can adjust prices based on the name's characteristics.
;; @params:
;;     name (buff 48): The name for which the price is being calculated.
;;     price-function (tuple): A tuple containing the parameters of the pricing function, including:
;;         buckets (list 16 uint): A list defining price multipliers for different name lengths.
;;         base (uint): The base price multiplier.
;;         coeff (uint): A coefficient that adjusts the base price.
;;         nonalpha-discount (uint): A discount applied to names containing non-alphabetic characters.
;;         no-vowel-discount (uint): A discount applied to names lacking vowel characters.
(define-private (compute-name-price
        (name (buff 48))
        (price-function {
            buckets: (list 16 uint),
            base: uint,
            coeff: uint,
            nonalpha-discount: uint,
            no-vowel-discount: uint,
        })
    )
    (let (
            ;; Determine the appropriate exponent based on the name's length.
            ;; This corresponds to a specific bucket in the pricing function.
            ;; The length of the name is used to index into the buckets list, with a maximum index of 15.
            (exponent (get-exp-at-index (get buckets price-function)
                (min u15 (- (len name) u1))
            ))
            ;; Calculate the no-vowel discount.
            ;; If the name has no vowels, apply the no-vowel discount from the price function.
            ;; Otherwise, use 1 indicating no discount.
            (no-vowel-discount (if (not (has-vowels-chars name))
                (get no-vowel-discount price-function)
                u1
            ))
            ;; Calculate the non-alphabetic character discount.
            ;; If the name contains non-alphabetic characters, apply the non-alpha discount from the price function.
            ;; Otherwise, use 1 indicating no discount.
            (nonalpha-discount (if (has-nonalpha-chars name)
                (get nonalpha-discount price-function)
                u1
            ))
            (len-name (len name))
        )
        (asserts! (> len-name u0) ERR-NAME-BLANK)
        ;; Compute the final price.
        ;; The base price, adjusted by the coefficient and exponent, is divided by the greater of the two discounts (non-alpha or no-vowel).
        ;; The result is then multiplied by 10 to adjust for unit precision.
        (ok (*
            (/
                (* (get coeff price-function)
                    (pow (get base price-function) exponent)
                )
                (max nonalpha-discount no-vowel-discount)
            )
            u10
        ))
    )
)

;; This function is similar to the 'transfer' function but does not check that the owner is the contract-caller.
;; @param id: the id of the nft being transferred.
;; @param owner: the principal of the current owner of the nft being transferred.
;; @param recipient: the principal of the recipient to whom the nft is being transferred.
(define-private (purchase-transfer
        (id uint)
        (owner principal)
        (recipient principal)
    )
    (let (
            ;; Attempts to retrieve the name and namespace associated with the given NFT ID.
            (name-and-namespace (unwrap! (map-get? index-to-name id) ERR-NO-NAME))
            ;; Retrieves the properties of the name within the namespace.
            (name-props (unwrap! (map-get? name-properties name-and-namespace) ERR-NO-NAME))
        )
        ;; Check owner and recipient is not the same
        (asserts! (not (is-eq owner recipient)) ERR-OPERATION-UNAUTHORIZED)
        (asserts! (is-eq owner (get owner name-props)) ERR-NOT-AUTHORIZED)
        ;; Update primary name if needed for owner
        (update-primary-name-owner id owner)
        ;; Update primary name if needed for recipient
        (update-primary-name-recipient id recipient)
        ;; Updates the owner to the recipient.
        (map-set name-properties name-and-namespace
            (merge name-props { owner: recipient })
        )
        ;; Executes the NFT transfer from the current owner to the recipient.
        (try! (nft-transfer? BNS-V2 id owner recipient))
        (print {
            topic: "transfer-name",
            owner: recipient,
            name: {
                name: (get name name-and-namespace),
                namespace: (get namespace name-and-namespace),
            },
            id: id,
            properties: (map-get? name-properties {
                name: (get name name-and-namespace),
                namespace: (get namespace name-and-namespace),
            }),
        })
        (ok true)
    )
)

;; Private function to update the primary name of an address when transfering a name
;; If the id is = to the primary name then it means that a transfer is happening and we should delete it
(define-private (update-primary-name-owner
        (id uint)
        (owner principal)
    )
    ;; Check if the owner is transferring the primary name
    (if (is-eq (map-get? primary-name owner) (some id))
        ;; If it is, then delete the primary name map
        (map-delete primary-name owner)
        ;; If it is not, do nothing, keep the current primary name
        false
    )
)

;; Private function to update the primary name of an address when receiving
(define-private (update-primary-name-recipient
        (id uint)
        (recipient principal)
    )
    ;; Check if recipient has a primary name
    (match (map-get? primary-name recipient)
        recipient-primary-name
        ;; If recipient has a primary name do nothing
        true
        ;; If recipient doesn't have a primary name
        (map-set primary-name recipient id)
    )
)

(define-private (handle-existing-name
        (name-props {
            registered-at: (optional uint),
            imported-at: (optional uint),
            hashed-salted-fqn-preorder: (optional (buff 20)),
            preordered-by: (optional principal),
            renewal-height: uint,
            stx-burn: uint,
            owner: principal,
        })
        (hashed-salted-fqn (buff 20))
        (contract-caller-preorder-height uint)
        (stx-burned uint)
        (name (buff 48))
        (namespace (buff 20))
        (renewal uint)
    )
    (let (
            ;; Retrieve the index of the existing name
            (name-index (unwrap-panic (map-get? name-to-index {
                name: name,
                namespace: namespace,
            })))
        )
        ;; Straight up check if the name was imported
        (asserts! (is-none (get imported-at name-props)) ERR-IMPORTED-BEFORE)
        ;; If the check passes then it is registered, we can straight up check the hashed-salted-fqn-preorder
        (match (get hashed-salted-fqn-preorder name-props)
            fqn
            ;; Compare both preorder's height
            (asserts!
                (>
                    (unwrap-panic (get created-at
                        (map-get? name-preorders {
                            hashed-salted-fqn: fqn,
                            buyer: (unwrap-panic (get preordered-by name-props)),
                        })
                    ))
                    contract-caller-preorder-height
                )
                ERR-PREORDERED-BEFORE
            )
            ;; Compare registered with preorder height
            (asserts!
                (> (unwrap-panic (get registered-at name-props))
                    contract-caller-preorder-height
                )
                ERR-FAST-MINTED-BEFORE
            )
        )
        ;; Update the name properties with the new preorder information since it is the best preorder
        (map-set name-properties {
            name: name,
            namespace: namespace,
        }
            (merge name-props
                {
                    hashed-salted-fqn-preorder: (some hashed-salted-fqn),
                    preordered-by: (some contract-caller),
                    registered-at: (some burn-block-height),
                    renewal-height: (if (is-eq renewal u0)
                        u0
                        (+ burn-block-height renewal)
                    ),
                    stx-burn: stx-burned,
                })
        )
        (try! (as-contract (stx-transfer? stx-burned .BNS-V2 (get owner name-props))))
        ;; Transfer ownership of the name to the new owner
        (try! (purchase-transfer name-index (get owner name-props) contract-caller))
        ;; Log the name transfer event
        (print {
            topic: "transfer-name",
            owner: contract-caller,
            name: {
                name: name,
                namespace: namespace,
            },
            id: name-index,
            properties: (map-get? name-properties {
                name: name,
                namespace: namespace,
            }),
        })
        ;; Return the name index
        (ok name-index)
    )
)

(define-private (register-new-name
        (id-to-be-minted uint)
        (hashed-salted-fqn (buff 20))
        (stx-burned uint)
        (name (buff 48))
        (namespace (buff 20))
        (lifetime uint)
    )
    (begin
        ;; Set the properties for the newly registered name
        (map-set name-properties
            {
                name: name,
                namespace: namespace,
            }
            {
                registered-at: (some burn-block-height),
                imported-at: none,
                hashed-salted-fqn-preorder: (some hashed-salted-fqn),
                preordered-by: (some contract-caller),
                renewal-height: (if (is-eq lifetime u0)
                    u0
                    (+ burn-block-height lifetime)
                ),
                stx-burn: stx-burned,
                owner: contract-caller,
            })
        ;; Update the index-to-name and name-to-index mappings
        (map-set index-to-name id-to-be-minted {
            name: name,
            namespace: namespace,
        })
        (map-set name-to-index {
            name: name,
            namespace: namespace,
        }
            id-to-be-minted
        )
        ;; Increment the BNS index
        (var-set bns-index id-to-be-minted)
        ;; Update the primary name for the new owner if necessary
        (update-primary-name-recipient id-to-be-minted contract-caller)
        ;; Mint a new NFT for the BNS name
        (try! (nft-mint? BNS-V2 id-to-be-minted contract-caller))
        ;; Burn the STX paid for the name registration
        (try! (as-contract (stx-burn? stx-burned .BNS-V2)))
        ;; Log the new name registration event
        (print {
            topic: "new-name",
            owner: contract-caller,
            name: {
                name: name,
                namespace: namespace,
            },
            id: id-to-be-minted,
            properties: (map-get? name-properties {
                name: name,
                namespace: namespace,
            }),
        })
        ;; Return the ID of the newly minted name
        (ok id-to-be-minted)
    )
)

;; Migration Functions
(define-public (namespace-airdrop
        (namespace (buff 20))
        (pricing {
            base: uint,
            buckets: (list 16 uint),
            coeff: uint,
            no-vowel-discount: uint,
            nonalpha-discount: uint,
        })
        (lifetime uint)
        (namespace-import principal)
        (namespace-manager (optional principal))
        (can-update-price bool)
        (manager-transfers bool)
        (manager-frozen bool)
        (revealed-at uint)
        (launched-at uint)
    )
    (begin
        ;; Check if migration is complete
        (asserts! (not (var-get migration-complete)) ERR-MIGRATION-IN-PROGRESS)
        ;; Ensure the contract-caller is the airdrop contract.
        (asserts! (is-eq DEPLOYER tx-sender) ERR-OPERATION-UNAUTHORIZED)
        ;; Ensure the namespace consists of valid characters only.
        (asserts! (not (has-invalid-chars namespace)) ERR-CHARSET-INVALID)
        ;; Check that the namespace is available for reveal.
        (asserts!
            (unwrap! (can-namespace-be-registered namespace)
                ERR-NAMESPACE-ALREADY-EXISTS
            )
            ERR-NAMESPACE-ALREADY-EXISTS
        )
        ;; Set all properties
        (map-set namespaces namespace
            {
                namespace-manager: namespace-manager,
                manager-transferable: manager-transfers,
                manager-frozen: manager-frozen,
                namespace-import: namespace-import,
                revealed-at: revealed-at,
                launched-at: (some launched-at),
                lifetime: lifetime,
                can-update-price-function: can-update-price,
                price-function: pricing,
            })
        ;; Emit an event to indicate the namespace is now ready and launched.
        (print {
            namespace: namespace,
            status: "launch",
            properties: (map-get? namespaces namespace),
        })
        ;; Confirm successful airdrop of the namespace
        (ok namespace)
    )
)

(define-public (name-airdrop
        (name (buff 48))
        (namespace (buff 20))
        (registered-at uint)
        (lifetime uint)
        (owner principal)
    )
    (let ((mint-index (+ u1 (var-get bns-index))))
        ;; Check if migration is complete
        (asserts! (not (var-get migration-complete)) ERR-MIGRATION-IN-PROGRESS)
        ;; Ensure the contract-caller is the airdrop contract.
        (asserts! (is-eq DEPLOYER tx-sender) ERR-OPERATION-UNAUTHORIZED)
        ;; Set all properties
        (map-set name-to-index {
            name: name,
            namespace: namespace,
        }
            mint-index
        )
        (map-set index-to-name mint-index {
            name: name,
            namespace: namespace,
        })
        (map-set name-properties {
            name: name,
            namespace: namespace,
        }
            {
                registered-at: (some registered-at),
                imported-at: none,
                hashed-salted-fqn-preorder: none,
                preordered-by: none,
                renewal-height: (if (is-eq lifetime u0)
                    u0
                    (+ burn-block-height lifetime)
                ),
                stx-burn: u0,
                owner: owner,
            })
        ;; Update the index
        (var-set bns-index mint-index)
        ;; Update the primary name of the recipient
        (map-set primary-name owner mint-index)
        ;; Mint the Name to the owner
        (try! (nft-mint? BNS-V2 mint-index owner))
        (print {
            topic: "new-airdrop",
            owner: owner,
            name: {
                name: name,
                namespace: namespace,
            },
            id: mint-index,
            registered-at: registered-at,
        })
        ;; Confirm successful airdrop of the namespace
        (ok mint-index)
    )
)

(define-public (flip-migration-complete)
    (ok (begin
        (asserts! (is-eq contract-caller DEPLOYER) ERR-NOT-AUTHORIZED)
        (var-set migration-complete true)
    ))
)
