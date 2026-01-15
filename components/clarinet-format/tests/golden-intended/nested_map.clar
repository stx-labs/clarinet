(define-public (mng-name-register)
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
)
