#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;

use reedline::{Reedline, DefaultPrompt, ReedlineEvent};

#[derive(Arbitrary, Clone, Debug)]
enum Action {
    ReedlineEvent(ReedlineEvent),
}

fuzz_target!(|data: Vec<Action>| {
    let mut line_editor = Reedline::create();
    let prompt = DefaultPrompt::default();

    for action in data {
        match action {
            Action::ReedlineEvent(event) => {
                line_editor.handle_event(&prompt, event).unwrap();
            }
        }
    }
});
