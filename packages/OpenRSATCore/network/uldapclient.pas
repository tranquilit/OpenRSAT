unit uldapclient;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  mormot.core.base,
  mormot.net.ldap;

type

  /// defines the transport layer used for an LDAP connection
  TLdapConnectionTransport = (
    /// LDAP communication protected by TLS
    ldctTls,
    /// unencrypted LDAP communication
    ldctPlain
  );

  /// defines the authentication mechanism used for an LDAP bind
  TLdapAuthenticationMode = (
    /// anonymous bind without any credentials
    ldamAnonymous,
    /// simple bind using a user name and password
    ldamSimple,
    /// SASL DIGEST authentication
    ldamSaslDigest,
    /// Kerberos authentication
    ldamKerberos
  );

  /// stores the credentials and authentication options used for an LDAP bind

  { TLdapCredentials }

  TLdapCredentials = record
    /// user name or bind identity
    UserName: RawUtf8;
    /// associated password
    // - stored as SpiUtf8 to benefit from mORMot sensitive string handling
    Password: SpiUtf8;
    /// authentication mechanism to use
    Authentication: TLdapAuthenticationMode;
    /// allow a password to be sent over an unprotected LDAP connection
    // - should normally remain false unless explicitly required
    AllowUnsafePasswordBind: Boolean;
    /// disable Kerberos channel binding validation
    KerberosDisableChannelBinding: Boolean;
    /// Kerberos signing and sealing mode
    KerberosSignSeal: TLdapKerberosSignSeal;
    /// optional Kerberos credential cache or credential file
    KerberosCredentialFile: RawUtf8;
    /// optional Kerberos authentication identity
    KerberosAuthIdentity: RawUtf8;
  end;

  /// stores all settings required to establish and maintain an LDAP connection

  { TLdapConnectionSettings }

  TLdapConnectionSettings = record
    /// LDAP server host name or address
    // - may be left empty when DiscoverWhenHostEmpty is enabled
    TargetHost: RawUtf8;
    /// LDAP server port
    Port: Integer;
    /// transport layer to use for the connection
    Transport: TLdapConnectionTransport;
    /// ignore TLS certificate validation errors
    // - should normally remain false in production
    IgnoreCertificateErrors: Boolean;
    /// enable automatic LDAP server discovery when TargetHost is empty
    DiscoverWhenHostEmpty: Boolean;
    /// use CLDAP when performing LDAP server discovery
    UseCldapDiscovery: Boolean;
    /// prefer the closest server reported by the discovery process
    SelectClosestServer: Boolean;
    /// try a TLS-capable connection before falling back to another transport
    TryTlsFirst: Boolean;
    /// delay in milliseconds applied during server discovery
    DiscoveryDelayMS: Integer;
    /// network operation timeout in milliseconds
    TimeoutMS: Integer;
    /// number of idle seconds before checking that the connection is alive
    PingIdleSeconds: Integer;
    /// automatically reconnect after a lost connection when possible
    AutoReconnect: Boolean;
    /// optional distinguished name used by Kerberos authentication
    KerberosDN: RawUtf8;
    /// optional Kerberos service principal name
    KerberosSPN: RawUtf8;
  end;

  /// identifies the high-level category of an LDAP operation failure
  // - intended to let application code handle errors without depending on
  // the exact LDAP result code or transport implementation
  TLdapErrorKind = (
    /// no error occurred
    lekNone,
    /// requested LDAP object could not be found
    lekNotFound,
    /// authentication failed
    lekAuthentication,
    /// authenticated identity is not allowed to perform the operation
    lekAuthorization,
    /// supplied request or data is invalid
    lekValidation,
    /// connection or network communication failed
    lekNetwork,
    /// TLS negotiation or certificate validation failed
    lekTls,
    /// LDAP server returned an error
    lekServer,
    /// operation was cancelled
    lekCancelled,
    /// error could not be mapped to a more specific category
    lekUnknown
  );

  /// contains the result of a generic LDAP operation
  TLdapOperationResult = record
    /// true when the operation completed successfully
    Success: Boolean;
    /// high-level error category
    ErrorKind: TLdapErrorKind;
    /// native LDAP result code when available
    // - zero usually denotes a successful LDAP operation
    LdapCode: Integer;
    /// human-readable error or status message
    Message: RawUtf8;
    /// elapsed operation time in milliseconds
    ElapsedTimeMS: Int64;
  end;

  /// optional limits and controls applied to an LDAP search request

  { TLdapSearchRequestOptions }

  TLdapSearchRequestOptions = record
    /// maximum number of entries to return
    // - zero may be used to keep the server/default implementation limit
    SizeLimit: Integer;
    /// maximum server-side search duration in seconds
    // - zero may be used to keep the server/default implementation limit
    TimeLimitSeconds: Integer;
    /// requested LDAP paged-search page size
    // - zero disables explicit paging
    PageSize: Integer;
    /// security descriptor sections requested from servers supporting
    // the corresponding LDAP control
    SearchSDFlags: TLdapSearchSDFlags;
  end;

  /// describes an LDAP search operation
  TLdapSearchRequest = record
    /// LDAP search scope
    Scope: TLdapSearchScope;
    /// distinguished name from which the search should start
    BaseDN: RawUtf8;
    /// attributes to retrieve for each matching entry
    // - an empty array may use the LDAP server default behavior
    Attributes: TRawUtf8DynArray;
    /// LDAP search filter
    Filter: RawUtf8;
    /// optional search limits and controls
    Options: TLdapSearchRequestOptions;
  end;

  /// stores all returned values for one LDAP attribute
  TLdapAttributeData = record
    /// LDAP attribute name
    Name: RawUtf8;
    /// raw values associated with this attribute
    // - LDAP attributes may contain either textual or binary data, therefore
    // values are deliberately exposed as RawByteString
    Values: TRawByteStringDynArray;
  end;

  /// pointer to an LDAP attribute data structure
  PLdapAttributeData = ^TLdapAttributeData;

  /// dynamic array of LDAP attributes
  TLdapAttributeDataDynArray = Array of TLdapAttributeData;

  /// stores one LDAP directory entry independently from the LDAP client
  // implementation
  TLdapEntryData = record
    /// full distinguished name of the LDAP entry
    DistinguishedName: RawUtf8;
    /// attributes returned for this entry
    Attributes: TLdapAttributeDataDynArray;
  end;

  /// pointer to an LDAP entry data structure
  PLdapEntryData = ^TLdapEntryData;

  /// dynamic array of LDAP directory entries
  TLdapEntryDataDynArray = Array of TLdapEntryData;

  /// identifies the overall outcome of an LDAP search operation
  TLdapSearchStatus = (
    /// search completed successfully
    lssOk,
    /// search completed but returned only a partial result
    lssPartial,
    /// search request was invalid before it could be executed
    lssInvalidRequest,
    /// connection to the LDAP server could not be established or was lost
    lssConnectionError,
    /// LDAP server returned an error for the search operation
    lssLdapError,
    /// unexpected client-side processing error
    lssInternalError
  );

  /// contains the complete result of an LDAP search operation
  TLdapSearchResult = record
    /// low-level operation result and possible LDAP error information
    OperationResult: TLdapOperationResult;
    /// entries returned by the LDAP server
    Entries: TLdapEntryDataDynArray;
  end;

  /// describes a request to create a new LDAP directory entry
  TLdapAddRequest = record
    /// distinguished name of the entry to create
    DistinguishedName: RawUtf8;
    /// initial attributes and values of the new entry
    Attributes: TLdapAttributeDataDynArray;
  end;

  /// describes one attribute modification within an LDAP modify request
  TLdapModifyChange = record
    /// modification operation, e.g. add, delete or replace
    Operation: TLdapModifyOp;
    /// attribute name and values involved in the modification
    Attribute: TLdapAttributeData;
  end;

  /// dynamic array of LDAP attribute modifications
  TLdapModifyChanges = Array of TLdapModifyChange;

  /// describes a set of modifications to apply to an existing LDAP entry
  TLdapModifyRequest = record
    /// distinguished name of the entry to modify
    DistinguishedName: RawUtf8;
    /// ordered list of attribute modifications to apply
    Changes: TLdapModifyChanges;
  end;

  /// describes a request to delete an LDAP directory entry
  TLdapDeleteRequest = record
    /// distinguished name of the entry to delete
    DistinguishedName: RawUtf8;
    /// also delete subordinate entries when supported by the implementation
    // - when false, deleting a non-leaf entry will normally fail
    DeleteChildren: Boolean;
  end;

  /// describes an LDAP ModifyDN operation used to rename or move an entry
  TLdapModifyDNRequest = record
    /// current distinguished name of the entry
    DistinguishedName: RawUtf8;
    /// new relative distinguished name
    NewRDN: RawUtf8;
    /// optional distinguished name of the new parent container
    // - an empty value keeps the entry under its current parent
    NewSuperior: RawUtf8;
    /// remove the old RDN attribute value after the rename operation
    DeleteOldRDN: Boolean;
  end;

  TLdapConnectionState = (
    lcsDisconnected,
    lcsConnected,
    lcsBound
  );

  { ************ LDAP Connection Abstraction }

  /// abstract access to an LDAP connection
  // - isolates LDAP transport and mormot.net.ldap implementation details from
  // application and domain code
  // - connection establishment and authentication are deliberately exposed as
  // separate operations
  ILdapConnection = Interface

    /// establish the network connection to an LDAP server
    // - ASettings defines discovery, transport, timeout and Kerberos settings
    // - does not imply that an LDAP bind has already been performed
    // - returns detailed status information instead of raising an expected
    // LDAP operation error
    function Connect(
      const ASettings: TLdapConnectionSettings): TLdapOperationResult;

    /// authenticate the current LDAP connection
    // - ACredentials selects anonymous, simple, SASL DIGEST or Kerberos bind
    // - Connect() should normally have succeeded before calling this method
    function Bind(
      const ACredentials: TLdapCredentials): TLdapOperationResult;

    /// close the current LDAP connection
    // - calling this method on an already disconnected instance should be safe
    procedure Disconnect;

    function State: TLdapConnectionState;

    /// execute an LDAP search request
    // - returns all retrieved entries together with the operation status
    function Search(
      const ARequest: TLdapSearchRequest): TLdapSearchResult;

    /// create a new LDAP directory entry
    function Add(
      const ARequest: TLdapAddRequest): TLdapOperationResult;

    /// modify attributes of an existing LDAP directory entry
    // - several add, delete or replace changes may be submitted in one request
    function Modify(
      const ARequest: TLdapModifyRequest): TLdapOperationResult;

    /// delete an LDAP directory entry
    function Delete(
      const ARequest: TLdapDeleteRequest): TLdapOperationResult;

    /// rename or move an existing LDAP directory entry
    function ModifyDN(
      const ARequest: TLdapModifyDNRequest): TLdapOperationResult;
  end;

implementation

end.

