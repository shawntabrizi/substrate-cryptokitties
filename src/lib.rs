#![cfg_attr(not(feature = "std"), no_std)]

/// A FRAME pallet template with necessary imports

/// Feel free to remove or edit this file as needed.
/// If you change the name of this file, make sure to update its references in runtime/src/lib.rs
/// If you remove this file, you can remove those references

/// For more guidance on Substrate FRAME, see the example pallet
/// https://github.com/paritytech/substrate/blob/master/frame/example/src/lib.rs

use codec::{Encode, Decode};
use frame_support::{decl_module, decl_storage, decl_event, decl_error};
use frame_support::traits::{Currency, Randomness, Get, EnsureOrigin};
use frame_system::{ensure_signed};
use sp_runtime::traits::{Zero, Hash};

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

/// The pallet's configuration trait.
pub trait Trait: frame_system::Trait {
	/// The overarching event type.
	type Event: From<Event<Self>> + Into<<Self as frame_system::Trait>::Event>;

	/// Standard currency interface.
	type Currency: Currency<Self::AccountId>;

	/// A source of on-chain randomness;
	type Randomness: Randomness<Self::Hash>;

	/// A configuration option that allows the pallet to store a lookup from user to tokens they own.
	/// Enabling this adds additional storage and computational overhead to your blockchain.
	///
	/// Enabled if `Some(value)` where `value` is the maximum number of tokens a user can own.
	type StoreUserTokens: Get<Option<u32>>;

	/// An origin that is allowed to mint net new tokens.
	type MintOrigin: EnsureOrigin<Self::Origin>;

	/// An origin that is allowed to claim a single token from the system.
	type ClaimOrigin: EnsureOrigin<Self::Origin>;
}

pub type BalanceOf<T> = <<T as Trait>::Currency as Currency<<T as frame_system::Trait>::AccountId>>::Balance;

#[derive(Encode, Decode, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "std", derive(Debug))]
pub struct Token<T: Trait> {
	pub owner: T::AccountId,
	pub price: Option<BalanceOf<T>>,
	pub deposit: BalanceOf<T>,
}

// This pallet's storage items.
decl_storage! {
	trait Store for Module<T: Trait> as NFT {
		// A list of all the tokens this pallet manages.
		Tokens get(fn token): map hasher(identity) T::Hash => Option<Token<T>>;
		// Optional storage that tracks a user and the tokens they own.
		TokensOf get(fn tokens_of): map hasher(twox_64_concat) T::AccountId => Option<Vec<T::Hash>>;
		// A list of users that have claimed their free token.
		Claimed get(fn claimed): map hasher(twox_64_concat) T::AccountId => bool;
	}
}

// The pallet's events
decl_event!(
	pub enum Event<T> where
		Hash = <T as frame_system::Trait>::Hash {
		TokenMinted(Hash),
	}
);

// The pallet's errors
decl_error! {
	pub enum Error for Module<T: Trait> {
		/// Value was None
		NoneValue,
		/// Value reached maximum and cannot be incremented further
		StorageOverflow,
	}
}

// The pallet's dispatchable functions.
decl_module! {
	/// The module declaration.
	pub struct Module<T: Trait> for enum Call where origin: T::Origin, system = frame_system {
		type Error = Error<T>;

		fn deposit_event() = default;

		#[weight = 0]
		fn mint(origin, recipient: T::AccountId) {
			T::MintOrigin::ensure_origin(origin)?;
			let token_id = Self::generate_token(&recipient);
			let token = Token {
				owner: recipient,
				price: None,
				deposit: Zero::zero(),
			};
			Self::add_token(token_id, &recipient)?;
			Self::deposit_event(RawEvent::TokenMinted(token_id));
		}

		// #[weight = 0]
		// pub fn transfer(origin, something: u32) -> dispatch::DispatchResult {
		// 	// Check it was signed and get the signer. See also: ensure_root and ensure_none
		// 	let who = ensure_signed(origin)?;

		// 	// Code to execute when something calls this.
		// 	// For example: the following line stores the passed in u32 in the storage
		// 	Something::put(something);

		// 	// Here we are raising the Something event
		// 	Self::deposit_event(RawEvent::SomethingStored(something, who));
		// 	Ok(())
		// }
	}
}

impl<T: Trait> Module<T> {
	fn generate_token(who: &T::AccountId) -> T::Hash {
		let seed = (
			T::Randomness::random(&[0]),
			who,
			frame_system::Module::<T>::extrinsic_index(),
			frame_system::Module::<T>::block_number(),
		);
		seed.using_encoded(T::Hashing::hash)
	}

	fn add_token_to_user(token: Token<T>, who: T::AccountId) -> DispatchResult {

	}
}
